/* File: main-cocoa.m */

/*
 * Copyright (c) 2011 Peter Ammon
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

#ifdef BOOL
# undef BOOL
#endif

/* Default creator signature */
#ifndef ANGBAND_CREATOR
# define ANGBAND_CREATOR 'Heng'
#endif

/* Mac headers */
#import <Cocoa/Cocoa.h>
#import <Carbon/Carbon.h> // For keycodes

static NSSize const AngbandScaleIdentity = {1.0, 1.0};
static NSString * const AngbandDirectoryNameLib = @"lib";
static NSString * const AngbandDirectoryNameBase = @"FrogComposband";

static NSString * const AngbandTerminalsDefaultsKey = @"Terminals";
static NSString * const AngbandTerminalRowsDefaultsKey = @"Rows";
static NSString * const AngbandTerminalColumnsDefaultsKey = @"Columns";
static NSInteger const AngbandWindowMenuItemTagBase = 1000;
static NSInteger const AngbandCommandMenuItemTagBase = 2000;

/* We can blit to a large layer or image and then scale it down during live resize, which makes resizing much faster, at the cost of some image quality during resizing */
#ifndef USE_LIVE_RESIZE_CACHE
# define USE_LIVE_RESIZE_CACHE 1
#endif

/* The max number of glyphs we support */
#define GLYPH_COUNT 256
#define MAX_COLORS 256
#define MSG_MAX SOUND_MAX

#define ANGBAND_TERM_MAX 8

/**
 * Specifies what kind of thing a file is, when writing.  See file_open().
 */
typedef enum
{
    FTYPE_TEXT = 1,
    FTYPE_SAVE,
    FTYPE_RAW,
    FTYPE_HTML
} file_type;

static bool new_game = TRUE;

/* Application defined event numbers */
enum
{
    AngbandEventWakeup = 1
};

/* Delay handling of pre-emptive "quit" event */
static BOOL quit_when_ready = FALSE;

/* Whether or not we allow sounds (only relevant for the screensaver, where the user can't configure it in-game) */
static BOOL allow_sounds = YES;

/* Set to indicate the game is over and we can quit without delay */
static Boolean game_is_finished = FALSE;

static int frames_per_second;

static NSFont *default_font;

@class AngbandView;

/* An AngbandContext represents a logical Term (i.e. what Angband thinks is a window). This typically maps to one NSView, but may map to more than one NSView (e.g. the Test and real screen saver view). */
@interface AngbandContext : NSObject <NSWindowDelegate>
{
@public
    
    /* The Angband term */
    term *terminal;
    
    /* Column and row cont, by default 80 x 24 */
    size_t cols;
    size_t rows;
    
    /* The size of the border between the window edge and the contents */
    NSSize borderSize;
    
    /* Our array of views */
    NSMutableArray *angbandViews;
    
    /* The buffered image */
    CGLayerRef angbandLayer;
    
    /* The font of this context */
    NSFont *angbandViewFont;
    
    /* If this context owns a window, here it is */
    NSWindow *primaryWindow;
    
    /* "Glyph info": an array of the CGGlyphs and their widths corresponding to the above font. */
    CGGlyph glyphArray[GLYPH_COUNT];
    CGFloat glyphWidths[GLYPH_COUNT];
    
    /* The size of one tile */
    NSSize tileSize;
    
    /* Font's descender */
    CGFloat fontDescender;
    
    /* Whether we are currently in live resize, which affects how big we render our image */
    int inLiveResize;
    
    /* Last time we drew, so we can throttle drawing */
    CFAbsoluteTime lastRefreshTime;
    
    /* To address subpixel rendering overdraw problems, we cache all the characters and attributes we're told to draw */
    wchar_t *charOverdrawCache;
    int *attrOverdrawCache;

@private

    BOOL _hasSubwindowFlags;
}

@property (nonatomic, assign) BOOL hasSubwindowFlags;

- (void)drawRect:(NSRect)rect inView:(NSView *)view;

/* Called at initialization to set the term */
- (void)setTerm:(term *)t;

/* Called when the context is going down. */
- (void)dispose;

- (NSSize)imageSize;

/* Return the rect for a tile at given coordinates. */
- (NSRect)rectInImageForTileAtX:(int)x Y:(int)y;

/* Draw the given wide character into the given tile rect. */
- (void)drawWChar:(wchar_t)wchar inRect:(NSRect)tile;

/* Locks focus on the Angband image, and scales the CTM appropriately. */
- (CGContextRef)lockFocus;

/* Locks focus on the Angband image but does NOT scale the CTM. Appropriate for drawing hairlines. */
- (CGContextRef)lockFocusUnscaled;

- (void)unlockFocus;

/* Returns the primary window for this angband context, creating it if necessary */
- (NSWindow *)makePrimaryWindow;

/* Called to add a new Angband view */
- (void)addAngbandView:(AngbandView *)view;

/* Make the context aware that one of its views changed size */
- (void)angbandViewDidScale:(AngbandView *)view;

/* Handle becoming the main window */
- (void)windowDidBecomeMain:(NSNotification *)notification;

/* Order the context's primary window frontmost */
- (void)orderFront;

/* Order the context's primary window out */
- (void)orderOut;

/* Return whether the context's primary window is ordered in or not */
- (BOOL)isOrderedIn;

/* Return whether the context's primary window is key */
- (BOOL)isMainWindow;

/* Invalidate the whole image */
- (void)setNeedsDisplay:(BOOL)val;

/* Invalidate part of the image, with the rect expressed in base coordinates */
- (void)setNeedsDisplayInBaseRect:(NSRect)rect;

/* Display (flush) our Angband views */
- (void)displayIfNeeded;

/* Called from the view to indicate that it is starting or ending live resize */
- (void)viewWillStartLiveResize:(AngbandView *)view;
- (void)viewDidEndLiveResize:(AngbandView *)view;

/* Class methods */

+ (void)beginGame;
+ (void)endGame;
- (AngbandView *)activeView;

@end

/**
 *  Generate a mask for the subwindow flags. The mask is just a safety check to make sure that our windows show and hide as expected.
 *  This function allows for future changes to the set of flags without needed to update it here (unless the underlying types change).
 */
u32b AngbandMaskForValidSubwindowFlags(void)
{
    int maxBits = 16;
    u32b mask = 0;

    for( int i = 0; i < maxBits; i++ )
    {
        if( window_flag_desc[i] != NULL )
        {
            mask |= (1 << i);
        }
    }

    return mask;
}

/**
 *  Check for changes in the subwindow flags and update window visibility. This seems to be called for every user event, so we don't
 *  want to do any unnecessary hiding or showing of windows.
 */
static void AngbandUpdateWindowVisibility(void)
{
    // because this function is called frequently, we'll make the mask static. it doesn't change between calls, since the flags themselves are hardcoded.
    static u32b validWindowFlagsMask = 0;

    if( validWindowFlagsMask == 0 )
    {
        validWindowFlagsMask = AngbandMaskForValidSubwindowFlags();
    }

    // loop through all of the subwindows and see if there is a change in the flags. if so, show or hide the corresponding window.
    // we don't care about the flags themselves; we just want to know if any are set.
    for( int i = 1; i < ANGBAND_TERM_MAX; i++ )
    {
        AngbandContext *angbandContext = angband_term[i]->data;

        if( angbandContext == nil ) continue;

        BOOL termHasSubwindowFlags = ((window_flag[i] & validWindowFlagsMask) > 0);

        if( angbandContext.hasSubwindowFlags && !termHasSubwindowFlags )
        {
            [angbandContext->primaryWindow close];
            angbandContext.hasSubwindowFlags = NO;
        }
        else if( !angbandContext.hasSubwindowFlags && termHasSubwindowFlags )
        {
            [angbandContext->primaryWindow orderFront: nil];
            angbandContext.hasSubwindowFlags = YES;
        }
    }

    // make the main window key so that user events go to the right spot
    AngbandContext *mainWindow = angband_term[0]->data;
    [mainWindow->primaryWindow makeKeyAndOrderFront: nil];
}

/* To indicate that a grid element contains a picture, we store 0xFFFF. */
#define NO_OVERDRAW ((wchar_t)(0xFFFF))

/* Here is some support for rounding to pixels in a scaled context */
static double push_pixel(double pixel, double scale, BOOL increase)
{
  double scaledPixel = pixel * scale;
  
  double roundedPixel = round(scaledPixel);
  if (fabs(roundedPixel - scaledPixel) <= .0001) scaledPixel = roundedPixel;
  else scaledPixel = (increase ? ceil : floor)(scaledPixel);
  
  return scaledPixel / scale;
}

/* Descriptions of how to "push pixels" in a given rect to integralize. For example, PUSH_LEFT means that we round expand the left edge if set, otherwise we shrink it. */
enum
{
  PUSH_LEFT = 0x1,
  PUSH_RIGHT = 0x2,
  PUSH_BOTTOM = 0x4,
  PUSH_TOP = 0x8
};

/* Return a rect whose border is in the "cracks" between tiles */
static NSRect crack_rect(NSRect rect, NSSize scale, unsigned pushOptions)
{
  double leftPixel   = push_pixel(NSMinX(rect), scale.width,   ! (pushOptions & PUSH_LEFT));
  double bottomPixel = push_pixel(NSMinY(rect), scale.height,  ! (pushOptions & PUSH_BOTTOM));
  double rightPixel  = push_pixel(NSMaxX(rect), scale.width,  !! (pushOptions & PUSH_RIGHT));
  double topPixel    = push_pixel(NSMaxY(rect), scale.height, !! (pushOptions & PUSH_TOP));
  return NSMakeRect(leftPixel, bottomPixel, rightPixel - leftPixel, topPixel - bottomPixel);
}

/* Returns the pixel push options (describing how we round) for the tile at a given index. Currently it's pretty uniform! */
static unsigned push_options(unsigned x, unsigned y)
{
  return PUSH_TOP | PUSH_LEFT;
}

/*
 * Graphics support
 */

/*
 * Value used to signal that we using ASCII, not graphical tiles.
 */ 
#define GRAF_MODE_NONE 0

/*
 * Requested graphics mode (as a grafID).
 * The current mode is stored in current_graphics_mode.
 */
static int graf_mode_req = 0;

/*
 * Helper function to check the various ways that graphics can be enabled, guarding against NULL
 */
static BOOL graphics_are_enabled(void)
{
    return GRAPHICS_NONE;
}

/*
 * Hack -- game in progress
 */
static Boolean game_in_progress = FALSE;


#pragma mark Prototypes
static void wakeup_event_loop(void);
static void hook_plog(const char *str);
static void hook_quit(const char * str);
static void load_prefs(void);
static void load_sounds(void);
static void init_windows(void);
//static void play_sound(int event);
static BOOL check_events(int wait);
static BOOL send_event(NSEvent *event);
static void record_current_savefile(void);

/*
 * Available values for 'wait'
 */
#define CHECK_EVENTS_DRAIN -1
#define CHECK_EVENTS_NO_WAIT	0
#define CHECK_EVENTS_WAIT 1


/* Methods for getting the appropriate NSUserDefaults */
@interface NSUserDefaults (AngbandDefaults)
+ (NSUserDefaults *)angbandDefaults;
@end

@implementation NSUserDefaults (AngbandDefaults)
+ (NSUserDefaults *)angbandDefaults
{
    return [NSUserDefaults standardUserDefaults];
}
@end

/* Methods for pulling images out of the Angband bundle (which may be separate from the current bundle in the case of a screensaver */
@interface NSImage (AngbandImages)
+ (NSImage *)angbandImage:(NSString *)name;
@end

/* The NSView subclass that draws our Angband image */
@interface AngbandView : NSView
{
    IBOutlet AngbandContext *angbandContext;
}

- (void)setAngbandContext:(AngbandContext *)context;
- (AngbandContext *)angbandContext;

@end

@implementation NSImage (AngbandImages)

/* Returns an image in the resource directoy of the bundle containing the Angband view class. */
+ (NSImage *)angbandImage:(NSString *)name
{
    NSBundle *bundle = [NSBundle bundleForClass:[AngbandView class]];
    NSString *path = [bundle pathForImageResource:name];
    NSImage *result;
    if (path) result = [[[NSImage alloc] initByReferencingFile:path] autorelease];
    else result = nil;
    return result;
}

@end


@implementation AngbandContext

@synthesize hasSubwindowFlags=_hasSubwindowFlags;

- (NSFont *)selectionFont
{
    return angbandViewFont;
}

- (BOOL)useLiveResizeOptimization
{
    /* If we have graphics turned off, text rendering is fast enough that we don't need to use a live resize optimization. Note here we are depending on current_graphics_mode being NULL when in text mode. */
    return inLiveResize && graphics_are_enabled();
}

- (NSSize)baseSize
{
    /* We round the base size down. If we round it up, I believe we may end up with pixels that nobody "owns" that may accumulate garbage. In general rounding down is harmless, because any lost pixels may be sopped up by the border. */
    return NSMakeSize(floor(cols * tileSize.width + 2 * borderSize.width), floor(rows * tileSize.height + 2 * borderSize.height));
}

// qsort-compatible compare function for CGSizes
static int compare_advances(const void *ap, const void *bp)
{
    const CGSize *a = ap, *b = bp;
    return (a->width > b->width) - (a->width < b->width);
}

- (void)updateGlyphInfo
{
    // Update glyphArray and glyphWidths
    NSFont *screenFont = [angbandViewFont screenFont];

    // Generate a string containing each MacRoman character
    unsigned char latinString[GLYPH_COUNT];
    size_t i;
    for (i=0; i < GLYPH_COUNT; i++) latinString[i] = (unsigned char)i;
    
    // Turn that into unichar. Angband uses ISO Latin 1.
    unichar unicharString[GLYPH_COUNT] = {0};
    NSString *allCharsString = [[NSString alloc] initWithBytes:latinString length:sizeof latinString encoding:NSISOLatin1StringEncoding];
    [allCharsString getCharacters:unicharString range:NSMakeRange(0, MIN(GLYPH_COUNT, [allCharsString length]))];
    [allCharsString autorelease];
    
    // Get glyphs
    memset(glyphArray, 0, sizeof glyphArray);
    CTFontGetGlyphsForCharacters((CTFontRef)screenFont, unicharString, glyphArray, GLYPH_COUNT);
    
    // Get advances. Record the max advance.
    CGSize advances[GLYPH_COUNT] = {};
    CTFontGetAdvancesForGlyphs((CTFontRef)screenFont, kCTFontHorizontalOrientation, glyphArray, advances, GLYPH_COUNT);
    for (i=0; i < GLYPH_COUNT; i++) {
        glyphWidths[i] = advances[i].width;
    }
    
    // For good non-mono-font support, use the median advance. Start by sorting all advances.
    qsort(advances, GLYPH_COUNT, sizeof *advances, compare_advances);
    
    // Skip over any initially empty run
    size_t startIdx;
    for (startIdx = 0; startIdx < GLYPH_COUNT; startIdx++)
    {
        if (advances[startIdx].width > 0) break;
    }
    
    // Pick the center to find the median
    CGFloat medianAdvance = 0;
    if (startIdx < GLYPH_COUNT)
    { // In case we have all zero advances for some reason
        medianAdvance = advances[(startIdx + GLYPH_COUNT)/2].width;
    }
    
    // Record the descender
    fontDescender = [screenFont descender];
    
    // Record the tile size. Note that these are typically fractional values - which seems sketchy, but we end up scaling the heck out of our view anyways, so it seems to not matter.
    tileSize.width = medianAdvance;
    tileSize.height = [screenFont ascender] - [screenFont descender];
}

- (void)updateImage
{
    NSSize size = NSMakeSize(1, 1);
    
    AngbandView *activeView = [self activeView];
    if (activeView)
    {
        /* If we are in live resize, draw as big as the screen, so we can scale nicely to any size. If we are not in live resize, then use the bounds of the active view. */
        NSScreen *screen;
        if ([self useLiveResizeOptimization] && (screen = [[activeView window] screen]) != NULL)
        {
            size = [screen frame].size;
        }
        else
        {
            size = [activeView bounds].size;
        }
    }
    
    size.width = fmax(1, ceil(size.width));
    size.height = fmax(1, ceil(size.height));
    
    CGLayerRelease(angbandLayer);
    
    // make a bitmap context as an example for our layer
    CGColorSpaceRef cs = CGColorSpaceCreateDeviceRGB();
    CGContextRef exampleCtx = CGBitmapContextCreate(NULL, 1, 1, 8 /* bits per component */, 48 /* bytesPerRow */, cs, kCGImageAlphaNoneSkipFirst | kCGBitmapByteOrder32Host);
    CGColorSpaceRelease(cs);
    angbandLayer = CGLayerCreateWithContext(exampleCtx, *(CGSize *)&size, NULL);
    CFRelease(exampleCtx);

    [self lockFocus];
    [[NSColor blackColor] set];
    NSRectFill((NSRect){NSZeroPoint, [self baseSize]});
    [self unlockFocus];
}

- (void)requestRedraw
{
    if (! self->terminal) return;
    
    term *old = Term;
    Term_activate(self->terminal);
    Term_redraw();
    Term_fresh();
    Term_activate(old);
}

- (void)setTerm:(term *)t
{
    terminal = t;
}

- (void)viewWillStartLiveResize:(AngbandView *)view
{
#if USE_LIVE_RESIZE_CACHE
    if (inLiveResize < INT_MAX) inLiveResize++;
    else [NSException raise:NSInternalInconsistencyException format:@"inLiveResize overflow"];
    
    if (inLiveResize == 1 && graphics_are_enabled())
    {
        [self updateImage];
        
        [self setNeedsDisplay:YES]; //we'll need to redisplay everything anyways, so avoid creating all those little redisplay rects
        [self requestRedraw];
    }
#endif
}

- (void)viewDidEndLiveResize:(AngbandView *)view
{
#if USE_LIVE_RESIZE_CACHE
    if (inLiveResize > 0) inLiveResize--;
    else [NSException raise:NSInternalInconsistencyException format:@"inLiveResize underflow"];
    
    if (inLiveResize == 0 && graphics_are_enabled())
    {
        [self updateImage];
        
        [self setNeedsDisplay:YES]; //we'll need to redisplay everything anyways, so avoid creating all those little redisplay rects
        [self requestRedraw];
    }
#endif
}

/* If we're trying to limit ourselves to a certain number of frames per second, then compute how long it's been since we last drew, and then wait until the next frame has passed. */
- (void)throttle
{
    if (frames_per_second > 0)
    {
        CFAbsoluteTime now = CFAbsoluteTimeGetCurrent();
        CFTimeInterval timeSinceLastRefresh = now - lastRefreshTime;
        CFTimeInterval timeUntilNextRefresh = (1. / (double)frames_per_second) - timeSinceLastRefresh;
        
        if (timeUntilNextRefresh > 0)
        {
            usleep((unsigned long)(timeUntilNextRefresh * 1000000.));
        }
    }
    lastRefreshTime = CFAbsoluteTimeGetCurrent();
}

- (void)drawWChar:(wchar_t)wchar inRect:(NSRect)tile
{
    CGContextRef ctx = [[NSGraphicsContext currentContext] graphicsPort];
  
    CTFontRef screenFont = (CTFontRef)[angbandViewFont screenFont];
    CGFloat tileOffsetY = CTFontGetAscent(screenFont);
    CGFloat tileOffsetX = 0.0f;
    CGFloat scale = 1.0f;
    
    UniChar unicharString[2] = {(UniChar)wchar, 0};

    // Get glyph and advance
    CGGlyph thisGlyphArray[1] = { 0 };
    CGSize advances[1] = { { 0, 0 } };
    CTFontGetGlyphsForCharacters(screenFont, unicharString, thisGlyphArray, 1);
    CGGlyph glyph = thisGlyphArray[0];
    CTFontGetAdvancesForGlyphs(screenFont, kCTFontHorizontalOrientation, thisGlyphArray, advances, 1);
    CGSize advance = advances[0];
    
    /* If our font is not monospaced, some characters can exceed the tile width.
     * If our glyph is too wide, compute scale to compress it horizontally.
     * Otherwize, compute the offset to account for potentially undersized glyphs */
    if (advance.width <= NSWidth(tile))
      tileOffsetX = (NSWidth(tile) - advance.width)/2; // Our glyph fits, so we can just draw it, with an offset if undersized
    else
      scale = NSWidth(tile) / advance.width; // Our glyph doesn't fit, so we'll have to scale it down
  
  CGAffineTransform t = CGAffineTransformIdentity;
  
  // Flip the Y-axis to match Core Text coordinate system
  t = CGAffineTransformScale(t, 1.0f, -1.0f);
  
  // Position the baseline
  t = CGAffineTransformTranslate(t, tile.origin.x + tileOffsetX, -(tile.origin.y + tileOffsetY));
  
  // Apply horizontal compression if needed
  if (scale != 1.0f) t = CGAffineTransformScale(t, scale, 1.0f);
  
  CGContextSetTextMatrix(ctx, t);
  CGContextShowGlyphsWithAdvances(ctx, &glyph, &CGSizeZero, 1);
  
  // Restore to identity to avoid carryover
  CGContextSetTextMatrix(ctx, CGAffineTransformIdentity);
}

/* Indication that we're redrawing everything, so get rid of the overdraw cache. */
- (void)clearOverdrawCache
{
    memset(charOverdrawCache, 0, self->cols * self->rows * sizeof *charOverdrawCache);
    memset(attrOverdrawCache, 0, self->cols * self->rows * sizeof *attrOverdrawCache);
}

- (void)clearOverdrawCacheFromX:(int)x y:(int)y width:(int)n {
    memset(&charOverdrawCache[y * self->cols + x], 0, n * sizeof *charOverdrawCache);
}

/* Lock and unlock focus on our image or layer, setting up the CTM appropriately. */
- (CGContextRef)lockFocusUnscaled
{
    /* Create an NSGraphicsContext representing this CGLayer */
    CGContextRef ctx = CGLayerGetContext(angbandLayer);
    NSGraphicsContext *context = [NSGraphicsContext graphicsContextWithGraphicsPort:ctx flipped:NO];
    [NSGraphicsContext saveGraphicsState];
    [NSGraphicsContext setCurrentContext:context];
    CGContextSaveGState(ctx);
    return ctx;
}

- (void)unlockFocus
{
    /* Restore the graphics state */
    CGContextRef ctx = [[NSGraphicsContext currentContext] graphicsPort];
    CGContextRestoreGState(ctx);
    [NSGraphicsContext restoreGraphicsState];
}

- (NSSize)imageSize
{
    /* Return the size of our layer */
    CGSize result = CGLayerGetSize(angbandLayer);
    return NSMakeSize(result.width, result.height);
}

- (CGContextRef)lockFocus
{
    return [self lockFocusUnscaled];
}

- (NSRect)rectInImageForTileAtX:(int)x Y:(int)y
{
    return NSMakeRect(x * tileSize.width + borderSize.width, y * tileSize.height + borderSize.height, tileSize.width, tileSize.height);
}

- (void)setSelectionFont:(NSFont*)font adjustTerminal: (BOOL)adjustTerminal
{
    /* Record the new font */
    [font retain];
    [angbandViewFont release];
    angbandViewFont = font;
    
    [self updateGlyphInfo];
    
    if( adjustTerminal )
    {
        // adjust terminal to fit window with new font; save the new columns and rows since they could be changed
        NSRect contentRect = [self->primaryWindow contentRectForFrameRect: [self->primaryWindow frame]];
        [self resizeTerminalWithContentRect: contentRect saveToDefaults: YES];
    }
    
    [self updateImage];
    
    [self clearOverdrawCache];
    
    [self requestRedraw];
}

- (id)init
{
    if ((self = [super init]))
    {
        self->cols = 80;
        self->rows = 24;

        self->borderSize = NSMakeSize(2, 2);

        /* Allocate overdraw cache, unscanned and collectable. */
        self->charOverdrawCache = NSAllocateCollectable(self->cols * self->rows *sizeof *charOverdrawCache, 0);
        self->attrOverdrawCache = NSAllocateCollectable(self->cols * self->rows *sizeof *attrOverdrawCache, 0);
        
        /* Allocate our array of views */
        angbandViews = [[NSMutableArray alloc] init];
        
        /* Make the image. Since we have no views, it'll just be a puny 1x1 image. */
        [self updateImage];        
    }
    return self;
}

/* Destroy all the receiver's stuff. This is intended to be callable more than once. */
- (void)dispose
{
    terminal = NULL;
    
    /* Disassociate ourselves from our angbandViews */
    [angbandViews makeObjectsPerformSelector:@selector(setAngbandContext:) withObject:nil];
    [angbandViews release];
    angbandViews = nil;
    
    /* Destroy the layer/image */
    CGLayerRelease(angbandLayer);
    angbandLayer = NULL;

    /* Font */
    [angbandViewFont release];
    angbandViewFont = nil;
    
    /* Window */
    [primaryWindow setDelegate:nil];
    [primaryWindow close];
    [primaryWindow release];
    primaryWindow = nil;
    
    /* Free overdraw cache (unless we're GC, in which case it was allocated collectable) */
    if (! [NSGarbageCollector defaultCollector]) free(self->charOverdrawCache);
    self->charOverdrawCache = NULL;
    if (! [NSGarbageCollector defaultCollector]) free(self->attrOverdrawCache);
    self->attrOverdrawCache = NULL;
}

- (void)dealloc
{
    [self dispose];
    [super dealloc];
}

#pragma mark -
#pragma mark Directories and Paths Setup

/**
 *  Return the path for Angband's lib directory and bail if it isn't found. The lib directory should be in the bundle's resources directory, since it's copied when built.
 */
+ (NSString *)libDirectoryPath
{
    NSString *bundleLibPath = [[[NSBundle mainBundle] resourcePath] stringByAppendingPathComponent: AngbandDirectoryNameLib];
    BOOL isDirectory = NO;
    BOOL libExists = [[NSFileManager defaultManager] fileExistsAtPath: bundleLibPath isDirectory: &isDirectory];

    if( !libExists || !isDirectory )
    {
        NSLog( @"[%@ %@]: can't find %@/ in bundle: isDirectory: %d libExists: %d", NSStringFromClass( [self class] ), NSStringFromSelector( _cmd ), AngbandDirectoryNameLib, isDirectory, libExists );
        NSRunAlertPanel( @"Missing Resources", @"PosChengband was unable to find required resources and must quit. Please report a bug on the Angband forums.", @"Quit", nil, nil );
        exit( 0 );
    }

    // angband requires the trailing slash for the directory path
    return [bundleLibPath stringByAppendingString: @"/"];
}

/**
 *  Return the path for the directory where Angband should look for its standard user file tree.
 */
+ (NSString *)angbandDocumentsPath
{
    // angband requires the trailing slash, so we'll just add it here; NSString won't care about it when we use the base path for other things
    NSString *documents = [NSSearchPathForDirectoriesInDomains( NSDocumentDirectory, NSUserDomainMask, YES ) lastObject];

#if defined(SAFE_DIRECTORY)
    NSString *versionedDirectory = [NSString stringWithFormat: @"%@-%s", AngbandDirectoryNameBase, VERSION_STRING];
    return [[documents stringByAppendingPathComponent: versionedDirectory] stringByAppendingString: @"/"];
#else
    return [[documents stringByAppendingPathComponent: AngbandDirectoryNameBase] stringByAppendingString: @"/"];
#endif
}

/**
 *  Give Angband the base paths that should be used for the various directories it needs. It will create any needed directories.
 */
+ (void)prepareFilePathsAndDirectories
{
    char libpath[PATH_MAX + 1] = "\0";
    char basepath[PATH_MAX + 1] = "\0";

    [[self libDirectoryPath] getFileSystemRepresentation: libpath maxLength: sizeof(libpath)];
    [[self angbandDocumentsPath] getFileSystemRepresentation: basepath maxLength: sizeof(basepath)];

    init_file_paths( libpath, libpath, basepath );
    create_needed_dirs();
}

#pragma mark -

/* Entry point for initializing Angband */
+ (void)beginGame
{
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    
    /* Hooks in some "z-util.c" hooks */
    plog_aux = hook_plog;
    quit_aux = hook_quit;
    
    // initialize file paths
    [self prepareFilePathsAndDirectories];

    // load preferences
    load_prefs();
    
    // load sounds
    load_sounds();
    
    /* Prepare the windows */
    init_windows();
    
    /* Set up game event handlers */
    //init_display();
    
	  /* Register the sound hook */
	  //sound_hook = play_sound;
    
    /* Set the "system" type */
    ANGBAND_SYS = "mac";
    
    /* Initialize */
    init_angband();

    /* Initialize some save file stuff */
    player_egid = getegid();
    
    /* Handle pending events (most notably update) and flush input */
    Term_flush();
    
    /*
     * Play a game -- "new_game" is set by "new", "open" or the open document
     * even handler as appropriate
     */
        
    [pool drain];
    
    /* Wait for response */
    prt("[Choose 'New' or 'Open' from the 'File' menu]", 23, 17);
    while (!game_in_progress) (check_events(CHECK_EVENTS_WAIT)); 

    /* Play the game */
    play_game(new_game);
}

+ (void)endGame
{
    p_ptr->playing = FALSE;
    p_ptr->leaving = TRUE;
    quit_when_ready = TRUE;
}


- (IBAction)setGraphicsMode:(NSMenuItem *)sender
{
    /* We stashed the graphics mode ID in the menu item's tag */
    graf_mode_req = [sender tag];

    /* Stash it in UserDefaults */
    [[NSUserDefaults angbandDefaults] setInteger:graf_mode_req forKey:@"GraphicsID"];
    [[NSUserDefaults angbandDefaults] synchronize];
    
    if (game_in_progress)
    {
        /* Hack -- Force redraw */
        do_cmd_redraw();
        
        /* Wake up the event loop so it notices the change */
        wakeup_event_loop();
    }
}

- (void)addAngbandView:(AngbandView *)view
{
    if (! [angbandViews containsObject:view])
    {
        [angbandViews addObject:view];
        [self updateImage];
        [self setNeedsDisplay:YES]; //we'll need to redisplay everything anyways, so avoid creating all those little redisplay rects
        [self requestRedraw];
    }
}

/* We have this notion of an "active" AngbandView, which is the largest - the idea being that in the screen saver, when the user hits Test in System Preferences, we don't want to keep driving the AngbandView in the background.  Our active AngbandView is the widest - that's a hack all right. Mercifully when we're just playing the game there's only one view. */
- (AngbandView *)activeView
{
    if ([angbandViews count] == 1)
        return [angbandViews objectAtIndex:0];
    
    AngbandView *result = nil;
    float maxWidth = 0;
    for (AngbandView *angbandView in angbandViews)
    {
        float width = [angbandView frame].size.width;
        if (width > maxWidth)
        {
            maxWidth = width;
            result = angbandView;
        }
    }
    return result;
}

- (void)angbandViewDidScale:(AngbandView *)view
{
    /* If we're live-resizing with graphics, we're using the live resize optimization, so don't update the image. Otherwise do it. */
    if (! (inLiveResize && graphics_are_enabled()) && view == [self activeView])
    {
        [self updateImage];
        
        [self setNeedsDisplay:YES]; //we'll need to redisplay everything anyways, so avoid creating all those little redisplay rects
        [self requestRedraw];
    }
}


- (void)removeAngbandView:(AngbandView *)view
{
    if ([angbandViews containsObject:view])
    {
        [angbandViews removeObject:view];
        [self updateImage];
        [self setNeedsDisplay:YES]; //we'll need to redisplay everything anyways, so avoid creating all those little redisplay rects
        if ([angbandViews count]) [self requestRedraw];
    }
}


static NSMenuItem *superitem(NSMenuItem *self)
{
    NSMenu *supermenu = [[self menu] supermenu];
    int index = [supermenu indexOfItemWithSubmenu:[self menu]];
    if (index == -1) return nil;
    else return [supermenu itemAtIndex:index];
}


- (BOOL)validateMenuItem:(NSMenuItem *)menuItem
{
    if ([menuItem action] == @selector(setGraphicsMode:))
    {
        [menuItem setState: ([menuItem tag] == graf_mode_req)];
        return YES;
    }
    return YES;
}

- (NSWindow *)makePrimaryWindow
{
    if (! primaryWindow)
    {
        // this has to be done after the font is set, which it already is in term_init_cocoa()
        CGFloat width = self->cols * tileSize.width + borderSize.width * 2.0;
        CGFloat height = self->rows * tileSize.height + borderSize.height * 2.0;
        NSRect contentRect = NSMakeRect( 0.0, 0.0, width, height );
      
        // make every window other than the main window closable
        if( angband_term[0]->data == self )
        {
          NSUInteger styleMask = NSWindowStyleMaskTitled | NSWindowStyleMaskResizable | NSWindowStyleMaskMiniaturizable;
          primaryWindow = [[NSWindow alloc] initWithContentRect:contentRect styleMask: styleMask backing:NSBackingStoreBuffered defer:YES];
        } else {
          NSUInteger styleMask = NSWindowStyleMaskUtilityWindow | NSWindowStyleMaskTitled | NSWindowStyleMaskResizable | NSWindowStyleMaskMiniaturizable | NSWindowStyleMaskClosable;
          NSPanel *panel = [[NSPanel alloc] initWithContentRect:contentRect styleMask: styleMask backing:NSBackingStoreBuffered defer:YES];
          
          // Prevent panel from hiding when app loses focus
          [panel setHidesOnDeactivate:NO];
          
          primaryWindow = (NSWindow *)panel;
        }

        /* Not to be released when closed */
        [primaryWindow setReleasedWhenClosed:NO];
        [primaryWindow setExcludedFromWindowsMenu: YES]; // we're using custom window menu handling

        /* Make the view */
        AngbandView *angbandView = [[AngbandView alloc] initWithFrame:contentRect];
        [angbandView setAngbandContext:self];
        [angbandViews addObject:angbandView];
        [primaryWindow setContentView:angbandView];
        [angbandView release];

        /* We are its delegate */
        [primaryWindow setDelegate:self];

        /* Update our image, since this is probably the first angband view we've gotten. */
        [self updateImage];
    }
    return primaryWindow;
}



#pragma mark View/Window Passthrough

/* This is what our views call to get us to draw to the window */
- (void)drawRect:(NSRect)rect inView:(NSView *)view
{
    /* Take this opportunity to throttle so we don't flush faster than desird. */
    BOOL viewInLiveResize = [view inLiveResize];
    if (! viewInLiveResize) [self throttle];

    /* With a GLayer, use CGContextDrawLayerInRect */
    CGContextRef context = [[NSGraphicsContext currentContext] graphicsPort];
    NSRect bounds = [view bounds];
    if (viewInLiveResize) CGContextSetInterpolationQuality(context, kCGInterpolationLow);
    CGContextSetBlendMode(context, kCGBlendModeCopy);
    CGContextDrawLayerInRect(context, *(CGRect *)&bounds, angbandLayer);
    if (viewInLiveResize) CGContextSetInterpolationQuality(context, kCGInterpolationDefault);
}


- (void)orderFront
{
    [[[angbandViews lastObject] window] makeKeyAndOrderFront:self];
}

- (BOOL)isOrderedIn
{
    return [[[angbandViews lastObject] window] isVisible];
}

- (BOOL)isMainWindow
{
    return [[[angbandViews lastObject] window] isMainWindow];
}

- (void)orderOut
{
    [[[angbandViews lastObject] window] orderOut:self];
}

- (void)setNeedsDisplay:(BOOL)val
{
    for (NSView *angbandView in angbandViews)
    {
        [angbandView setNeedsDisplay:val];
    }
}

- (void)setNeedsDisplayInBaseRect:(NSRect)rect
{
    for (NSView *angbandView in angbandViews)
    {
        [angbandView setNeedsDisplayInRect: rect];
    }
}

- (void)displayIfNeeded
{
    [[self activeView] displayIfNeeded];
}

- (void)resizeOverdrawCache
{
    /* Free overdraw cache (unless we're GC, in which case it was allocated collectable) */
    if (! [NSGarbageCollector defaultCollector]) free(self->charOverdrawCache);
    self->charOverdrawCache = NULL;
    if (! [NSGarbageCollector defaultCollector]) free(self->attrOverdrawCache);
    self->attrOverdrawCache = NULL;

    /* Allocate overdraw cache, unscanned and collectable. */
    self->charOverdrawCache = NSAllocateCollectable(self->cols * self->rows *sizeof *charOverdrawCache, 0);
    self->attrOverdrawCache = NSAllocateCollectable(self->cols * self->rows *sizeof *attrOverdrawCache, 0);
}

- (void)resizeTerminalWithContentRect: (NSRect)contentRect saveToDefaults: (BOOL)saveToDefaults
{
    CGFloat newRows = floor( (contentRect.size.height - (borderSize.height * 2.0)) / tileSize.height );
    CGFloat newColumns = floor( (contentRect.size.width - (borderSize.width * 2.0)) / tileSize.width );

    self->cols = newColumns;
    self->rows = newRows;
    [self resizeOverdrawCache];

    if( saveToDefaults )
    {
        int termIndex = 0;
        for( termIndex = 0; termIndex < ANGBAND_TERM_MAX; termIndex++ )
        {
            if( angband_term[termIndex] == self->terminal ) break;
        }

        NSArray *terminals = [[NSUserDefaults standardUserDefaults] valueForKey: AngbandTerminalsDefaultsKey];

        if( termIndex < (int)[terminals count] )
        {
            NSMutableDictionary *mutableTerm = [[NSMutableDictionary alloc] initWithDictionary: [terminals objectAtIndex: termIndex]];
            [mutableTerm setValue: @(self->cols) forKey: AngbandTerminalColumnsDefaultsKey];
            [mutableTerm setValue: @(self->rows) forKey: AngbandTerminalRowsDefaultsKey];

            NSMutableArray *mutableTerminals = [[NSMutableArray alloc] initWithArray: terminals];
            [mutableTerminals replaceObjectAtIndex: termIndex withObject: mutableTerm];

            [[NSUserDefaults standardUserDefaults] setValue: mutableTerminals forKey: AngbandTerminalsDefaultsKey];
            [mutableTerminals release];
            [mutableTerm release];
        }
    }

    term *old = Term;
    Term_activate( self->terminal );
    Term_resize(MIN((int)newColumns, 255), MIN((int)newRows, 255));
    Term_redraw();
    Term_activate( old );
}

#pragma mark -
#pragma mark NSWindowDelegate Methods

//- (void)windowWillStartLiveResize: (NSNotification *)notification
//{
//}

- (void)windowDidEndLiveResize: (NSNotification *)notification
{
    NSWindow *window = [notification object];
    NSRect contentRect = [window contentRectForFrameRect: [window frame]];
    [self resizeTerminalWithContentRect: contentRect saveToDefaults: YES];
}

- (NSSize)windowWillResize:(NSWindow *)sender toSize:(NSSize)frameSize {
  // Get the content size (the drawable area, excluding title bar, etc.)
  NSSize contentSize = [sender contentRectForFrameRect:(NSRect){ .size = frameSize }].size;
  
  // Snap the content width to a multiple of tile width
  contentSize.width = roundf((contentSize.width - borderSize.width * 2) / tileSize.width) * tileSize.width + borderSize.width * 2;
  
  // Convert content size back to frame size
  NSRect frameRect = [sender frameRectForContentRect:(NSRect){ .size = contentSize }];
  return frameRect.size;
}

- (void)windowDidEnterFullScreen: (NSNotification *)notification
{
    NSWindow *window = [notification object];
    NSRect contentRect = [window contentRectForFrameRect: [window frame]];
    [self resizeTerminalWithContentRect: contentRect saveToDefaults: NO];
}

- (void)windowDidExitFullScreen: (NSNotification *)notification
{
    NSWindow *window = [notification object];
    NSRect contentRect = [window contentRectForFrameRect: [window frame]];
    [self resizeTerminalWithContentRect: contentRect saveToDefaults: NO];
}

- (void)windowDidBecomeMain:(NSNotification *)notification
{
    NSWindow *window = [notification object];

    if( window != self->primaryWindow ) return;

    int termIndex = 0;
    for( termIndex = 0; termIndex < ANGBAND_TERM_MAX; termIndex++ )
    {
        if( angband_term[termIndex] == self->terminal ) break;
    }

    NSMenuItem *item = [[[NSApplication sharedApplication] windowsMenu] itemWithTag: AngbandWindowMenuItemTagBase + termIndex];
    [item setState: NSOnState];

    if( [[NSFontPanel sharedFontPanel] isVisible] )
    {
        [[NSFontPanel sharedFontPanel] setPanelFont: [self selectionFont] isMultiple: NO];
    }
}

- (void)windowDidResignMain: (NSNotification *)notification
{
    NSWindow *window = [notification object];

    if( window != self->primaryWindow ) return;

    int termIndex = 0;
    for( termIndex = 0; termIndex < ANGBAND_TERM_MAX; termIndex++ )
    {
        if( angband_term[termIndex] == self->terminal ) break;
    }

    NSMenuItem *item = [[[NSApplication sharedApplication] windowsMenu] itemWithTag: AngbandWindowMenuItemTagBase + termIndex];
    [item setState: NSOffState];
}

@end


@implementation AngbandView

- (BOOL)isOpaque
{
    return YES;
}

- (BOOL)isFlipped
{
    return YES;
}

- (void)drawRect:(NSRect)rect
{
    if (! angbandContext)
    {
        /* Draw bright orange, 'cause this ain't right */
        [[NSColor orangeColor] set];
        NSRectFill([self bounds]);
    }
    else
    {
        /* Tell the Angband context to draw into us */
        [angbandContext drawRect:rect inView:self];
    }
}

- (void)setAngbandContext:(AngbandContext *)context
{
    angbandContext = context;
}

- (AngbandContext *)angbandContext
{
    return angbandContext;
}

- (void)setFrameSize:(NSSize)size
{
    BOOL changed = ! NSEqualSizes(size, [self frame].size);
    [super setFrameSize:size];
    if (changed) [angbandContext angbandViewDidScale:self];
}

- (void)viewWillStartLiveResize
{
    [angbandContext viewWillStartLiveResize:self];
}

- (void)viewDidEndLiveResize
{
    [angbandContext viewDidEndLiveResize:self];
}

@end

/*
 * Delay handling of double-clicked savefiles
 */
Boolean open_when_ready = FALSE;



/*** Some generic functions ***/

/* Sets an Angband color at a given index */
static void set_color_for_index(int idx)
{
  CGFloat rv, gv, bv;
  CGFloat gamma = 1.4f;
  
  rv = powf(angband_color_table[idx][1]/255.0f, 1.0f/gamma);
  gv = powf(angband_color_table[idx][2]/255.0f, 1.0f/gamma);
  bv = powf(angband_color_table[idx][3]/255.0f, 1.0f/gamma);
  
  CGContextSetRGBFillColor([[NSGraphicsContext currentContext] graphicsPort], rv, gv, bv, 1.0f);
}

/* Remember the current character in UserDefaults so we can select it by default next time. */
static void record_current_savefile(void)
{
    NSString *savefileString = [[NSString stringWithCString:savefile encoding:NSMacOSRomanStringEncoding] lastPathComponent];
    if (savefileString)
    {
        NSUserDefaults *angbandDefs = [NSUserDefaults angbandDefaults];
        [angbandDefs setObject:savefileString forKey:@"SaveFile"];
        [angbandDefs synchronize];        
    }
}


/*** Support for the "z-term.c" package ***/


/*
 * Initialize a new Term
 */
static void Term_init_cocoa(term *t)
{
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    AngbandContext *context = [[AngbandContext alloc] init];
    
    /* Give the term a hard retain on context (for GC) */
    t->data = (void *)CFRetain(context);
    [context release];
    
    /* Handle graphics */
    t->higher_pict = !! use_graphics;
    t->always_pict = FALSE;
    
    NSDisableScreenUpdates();
    
    /* Figure out the frame autosave name based on the index of this term */
    NSString *autosaveName = nil;
    int termIdx;
    for (termIdx = 0; termIdx < ANGBAND_TERM_MAX; termIdx++)
    {
        if (angband_term[termIdx] == t)
        {
            autosaveName = [NSString stringWithFormat:@"AngbandTerm-%d", termIdx];
            break;
        }
    }
    
    NSString *fontName = [[NSUserDefaults angbandDefaults] 
        stringForKey:[NSString stringWithFormat:@"FontName-%d", termIdx]];
    if (! fontName) fontName = [default_font fontName];
    float fontSize = [[NSUserDefaults angbandDefaults] 
        floatForKey:[NSString stringWithFormat:@"FontSize-%d", termIdx]];
    if (! fontSize) fontSize = [default_font pointSize];
    [context setSelectionFont:[NSFont fontWithName:fontName size:fontSize] adjustTerminal: NO];

    NSArray *terminalDefaults = [[NSUserDefaults standardUserDefaults] valueForKey: AngbandTerminalsDefaultsKey];
    NSInteger rows = 24;
    NSInteger columns = 80;

    if( termIdx < (int)[terminalDefaults count] )
    {
        NSDictionary *term = [terminalDefaults objectAtIndex: termIdx];
        rows = [[term valueForKey: AngbandTerminalRowsDefaultsKey] integerValue];
        columns = [[term valueForKey: AngbandTerminalColumnsDefaultsKey] integerValue];
    }
    
    context->cols = columns;
    context->rows = rows;
    [context resizeOverdrawCache];
    
    NSWindow *window = [context makePrimaryWindow];
    
    /* Set its title and, for auxiliary terms, tentative size */
    if (termIdx == 0)
    {
        [window setTitle:@"FrogComposband"];
    }
    else
    {
        [window setTitle:[NSString stringWithFormat:@"Term %d", termIdx]];
    }
    
    /* If this is the first term, and we support full screen (Mac OS X Lion or later), then allow it to go full screen (sweet). Allow other terms to be FullScreenAuxilliary, so they can at least show up. Unfortunately in Lion they don't get brought to the full screen space; but they would only make sense on multiple displays anyways so it's not a big loss. */
    if ([window respondsToSelector:@selector(toggleFullScreen:)])
    {
        NSWindowCollectionBehavior behavior = [window collectionBehavior];
        behavior |= (termIdx == 0 ? NSWindowCollectionBehaviorFullScreenPrimary : NSWindowCollectionBehaviorFullScreenAuxiliary);
        [window setCollectionBehavior:behavior];
    }
    
    /* No Resume support yet, though it would not be hard to add */
    if ([window respondsToSelector:@selector(setRestorable:)])
    {
        [window setRestorable:NO];
    }
    
    /* And maybe that's all for naught */
    if (autosaveName) [window setFrameAutosaveName:autosaveName];
    
    /* Tell it about its term. Do this after we've sized it so that the sizing doesn't trigger redrawing and such. */
    [context setTerm:t];
    
    /* Only order front if it's the first term. Other terms will be ordered front from update_term_visibility(). This is to work around a problem where Angband aggressively tells us to initialize terms that don't do anything! */
    if (t == angband_term[0]) [context orderFront];
    
    NSEnableScreenUpdates();
    
    /* Set "mapped" flag */
    t->mapped_flag = true;
    [pool drain];
}


/*
 * Nuke an old Term
 */
static void Term_nuke_cocoa(term *t)
{
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    
    AngbandContext *context = t->data;
    if (context)
    {
        /* Tell the context to get rid of its windows, etc. */
        [context dispose];
        
        /* Balance our CFRetain from when we created it */
        CFRelease(context);
        
        /* Done with it */
        t->data = NULL;
    }
    
    [pool drain];
}

/*
 * React to changes
 */
static errr Term_xtra_cocoa_react(void)
{
    /* Don't actually switch graphics until the game is running */
    if (!initialized || !game_in_progress) return (-1);

    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    //AngbandContext *angbandContext = Term->data;
#if 0    
    /* Handle graphics */
    int expected_graf_mode = (current_graphics_mode ? current_graphics_mode->grafID : GRAF_MODE_NONE);
    if (graf_mode_req != expected_graf_mode)
    {
        graphics_mode *new_mode;
		if (graf_mode_req != GRAF_MODE_NONE) {
			new_mode = get_graphics_mode(graf_mode_req);
		} else {
			new_mode = NULL;
        }
        
        /* Get rid of the old image. CGImageRelease is NULL-safe. */
        CGImageRelease(pict_image);
        pict_image = NULL;
        
        /* Try creating the image if we want one */
        if (new_mode != NULL)
        {
            NSString *img_name = [NSString stringWithCString:new_mode->file 
                                                encoding:NSMacOSRomanStringEncoding];
            pict_image = create_angband_image(img_name);

            /* If we failed to create the image, set the new desired mode to NULL */
            if (! pict_image)
                new_mode = NULL;
        }
        
        /* Record what we did */
        use_graphics = (new_mode != NULL);
        use_transparency = (new_mode != NULL);
        ANGBAND_GRAF = (new_mode ? new_mode->pref : NULL);
        current_graphics_mode = new_mode;
        
        /* Enable or disable higher picts. Note: this should be done for all terms. */
        angbandContext->terminal->higher_pict = !! use_graphics;
        
        if (pict_image && current_graphics_mode)
        {
            /* Compute the row and column count via the image height and width. */
            pict_rows = (int)(CGImageGetHeight(pict_image) / current_graphics_mode->cell_height);
            pict_cols = (int)(CGImageGetWidth(pict_image) / current_graphics_mode->cell_width);
        }
        else
        {
            pict_rows = 0;
            pict_cols = 0;
        }
        
        /* Reset visuals */
        if (initialized && game_in_progress)
        {
            reset_visuals(TRUE);
        }
    }
#endif    
    [pool drain];
    
    return 0;
}


/*
 * Do a "special thing"
 */
static errr Term_xtra_cocoa(int n, int v)
{
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    AngbandContext* angbandContext = Term->data;
    
    errr result = 0;
    
    /* Analyze */
    switch (n)
    {
        case TERM_XTRA_NOISE:
        {
            NSBeep();
            break;
        }
            
            /* Process random events */
        case TERM_XTRA_BORED:
        {
            // show or hide cocoa windows based on the subwindow flags set by the user
            AngbandUpdateWindowVisibility();

            /* Process an event */
            (void)check_events(CHECK_EVENTS_NO_WAIT);
            
            break;
        }
            
            /* Process pending events */
        case TERM_XTRA_EVENT:
        {
            /* Process an event */
            (void)check_events(v);
            
            break;
        }
            
            /* Flush all pending events (if any) */
        case TERM_XTRA_FLUSH:
        {
            /* Hack -- flush all events */
            while (check_events(CHECK_EVENTS_DRAIN)) /* loop */;
            
            break;
        }
            
            /* Hack -- Change the "soft level" */
        case TERM_XTRA_LEVEL:
        {
            /* Here we could activate (if requested), but I don't think Angband should be telling us our window order (the user should decide that), so do nothing. */            
            break;
        }
            
            /* Clear the screen */
        case TERM_XTRA_CLEAR:
        {        
            [angbandContext lockFocus];
            [[NSColor blackColor] set];
            NSRect imageRect = {NSZeroPoint, [angbandContext imageSize]};            
            NSRectFillUsingOperation(imageRect, NSCompositeCopy);
            [angbandContext unlockFocus];
            [angbandContext clearOverdrawCache];
            [angbandContext setNeedsDisplay:YES];
            break;
        }
            
            /* React to changes */
        case TERM_XTRA_REACT:
        {
            /* React to changes */
            return Term_xtra_cocoa_react();
        }
            
            /* Delay (milliseconds) */
        case TERM_XTRA_DELAY:
        {
            /* If needed */
            if (v > 0)
            {
                
                double seconds = v / 1000.;
                NSDate* date = [NSDate dateWithTimeIntervalSinceNow:seconds];
                do
                {
                    NSEvent* event;
                    do
                    {
                        event = [NSApp nextEventMatchingMask:-1 untilDate:date inMode:NSDefaultRunLoopMode dequeue:YES];
                        if (event) send_event(event);
                    } while (event);
                } while ([date timeIntervalSinceNow] >= 0);
                
            }
            
            break;
        }
            
        case TERM_XTRA_FRESH:
        {
            /* No-op -- see #1669 
             * [angbandContext displayIfNeeded]; */
            break;
        }
            
        default:
            /* Oops */
            result = 1;
            break;
    }
    
    [pool drain];
    
    return result;
}

static errr Term_curs_cocoa(int x, int y)
{
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    AngbandContext *angbandContext = Term->data;
    
    /* Get the tile */
    NSRect rect = [angbandContext rectInImageForTileAtX:x Y:y];
    
    /* We'll need to redisplay in that rect */
    NSRect redisplayRect = rect;

    /* Go to the pixel boundaries corresponding to this tile */
    rect = crack_rect(rect, AngbandScaleIdentity, push_options(x, y));
    
    /* Lock focus and draw it */
    [angbandContext lockFocus];
    [[NSColor yellowColor] set];
    NSFrameRectWithWidth(rect, 1);
    [angbandContext unlockFocus];
    
    /* Invalidate that rect */
    [angbandContext setNeedsDisplayInBaseRect:redisplayRect];
    
    [pool drain];
    return 0;
}

/*
 * Low level graphics (Assumes valid input)
 *
 * Erase "n" characters starting at (x,y)
 */
static errr Term_wipe_cocoa(int x, int y, int n)
{
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    AngbandContext *angbandContext = Term->data;
    
    /* clear our overdraw cache for subpixel rendering */
    [angbandContext clearOverdrawCacheFromX:x y:y width:n];
    
    /* Erase the block of characters */
    NSRect rect = [angbandContext rectInImageForTileAtX:x Y:y];
    
    /* Maybe there's more than one */
    if (n > 1) rect = NSUnionRect(rect, [angbandContext rectInImageForTileAtX:x + n-1 Y:y]);
    
    /* Lock focus and clear */
    [angbandContext lockFocus];
    [[NSColor blackColor] set];
    NSRectFill(rect);
    [angbandContext unlockFocus];    
    [angbandContext setNeedsDisplayInBaseRect:rect];
    
    [pool drain];
    
    return (0);
}
#if 0
static void draw_image_tile(CGImageRef image, NSRect srcRect, NSRect dstRect, NSCompositingOperation op)
{
    /* When we use high-quality resampling to draw a tile, pixels from outside the tile may bleed in, causing graphics artifacts. Work around that. */
    CGImageRef subimage = CGImageCreateWithImageInRect(image, *(CGRect *)&srcRect);
    NSGraphicsContext *context = [NSGraphicsContext currentContext];
    [context setCompositingOperation:op];
    CGContextDrawImage([context graphicsPort], *(CGRect *)&dstRect, subimage);
    CGImageRelease(subimage);
}

static errr Term_pict_cocoa(int x, int y, int n, const int *ap,
                            const wchar_t *cp, const int *tap,
                            const wchar_t *tcp)
{
    
    /* Paranoia: Bail if we don't have a current graphics mode */
    if (! current_graphics_mode) return -1;
    
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    AngbandContext* angbandContext = Term->data;

    /* Indicate that we have a picture here (and hence this should not be overdrawn by Term_text_cocoa) */
    angbandContext->charOverdrawCache[y * angbandContext->cols + x] = NO_OVERDRAW;
    
    /* Lock focus */
    [angbandContext lockFocus];
    
    NSRect destinationRect = [angbandContext rectInImageForTileAtX:x Y:y];

    /* Expand the rect to every touching pixel to figure out what to redisplay */
    NSRect redisplayRect = crack_rect(destinationRect, AngbandScaleIdentity, PUSH_RIGHT | PUSH_TOP | PUSH_BOTTOM | PUSH_LEFT);
    
    /* Expand our destinationRect */
    destinationRect = crack_rect(destinationRect, AngbandScaleIdentity, push_options(x, y));
    
    /* Scan the input */
    int i;
    int graf_width = current_graphics_mode->cell_width;
    int graf_height = current_graphics_mode->cell_height;

    for (i = 0; i < n; i++)
    {
        
        int a = *ap++;
        wchar_t c = *cp++;
        
        int ta = *tap++;
        wchar_t tc = *tcp++;
        
        
        /* Graphics -- if Available and Needed */
        if (use_graphics && (a & 0x80) && (c & 0x80))
        {
            int col, row;
            int t_col, t_row;
            

            /* Primary Row and Col */
            row = ((byte)a & 0x7F) % pict_rows;
            col = ((byte)c & 0x7F) % pict_cols;
            
            NSRect sourceRect;
            sourceRect.origin.x = col * graf_width;
            sourceRect.origin.y = row * graf_height;
            sourceRect.size.width = graf_width;
            sourceRect.size.height = graf_height;
            
            /* Terrain Row and Col */
            t_row = ((byte)ta & 0x7F) % pict_rows;
            t_col = ((byte)tc & 0x7F) % pict_cols;
            
            NSRect terrainRect;
            terrainRect.origin.x = t_col * graf_width;
            terrainRect.origin.y = t_row * graf_height;
            terrainRect.size.width = graf_width;
            terrainRect.size.height = graf_height;
            
            /* Transparency effect. We really want to check current_graphics_mode->alphablend, but as of this writing that's never set, so we do something lame.  */
            //if (current_graphics_mode->alphablend)
            if (graf_width > 8 || graf_height > 8)
            {
                draw_image_tile(pict_image, terrainRect, destinationRect, NSCompositeCopy);
                draw_image_tile(pict_image, sourceRect, destinationRect, NSCompositeSourceOver); 
            }
            else
            {
                draw_image_tile(pict_image, sourceRect, destinationRect, NSCompositeCopy);
            }
        }        
    }
    
    [angbandContext unlockFocus];
    [angbandContext setNeedsDisplayInBaseRect:redisplayRect];
    
    [pool drain];
    
    return (0);
}
#endif

/*
 * Low level graphics.  Assumes valid input.
 *
 * Draw several ("n") chars, with an attr, at a given location.
 */
static errr Term_text_cocoa(int x, int y, int n, byte a, cptr cp)
{
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

    /* Subpixel rendering looks really nice!  Unfortunately, drawing a string like this:
     .@
     causes subpixels to extend slightly into the region 'owned' by the period.  This means that when the user presses right,
     those subpixels 'owned' by the period above do not get redrawn by Angband, so we leave little blue and red subpixel turds
     all over the screen.  Turning off subpixel rendering fixes this, as does increasing the font advance by a pixel, but that is
     ugly.  Our hack solution is to remember all of the characters we draw as well as their locations and colors (in charOverdrawCache),
     and then re-blit the previous and next character (if any).
     */
    
    if(a >= MAX_COLORS) a=1;
    
    NSRect redisplayRect = NSZeroRect;
    AngbandContext* angbandContext = Term->data;
    
    /* record our data in our cache */
    int start = y * angbandContext->cols + x;
    for (int location = 0; location < n; location++) {
        angbandContext->charOverdrawCache[start + location] = cp[location];
        angbandContext->attrOverdrawCache[start + location] = a;
    }
    
    /* Focus on our layer */
    [angbandContext lockFocus];

    /* Starting pixel */
    NSRect charRect = [angbandContext rectInImageForTileAtX:x Y:y];
    
    const CGFloat tileWidth = angbandContext->tileSize.width;
    
    /* erase behind us */
    unsigned leftPushOptions = push_options(x, y);
    unsigned rightPushOptions = push_options(x + n - 1, y);
    leftPushOptions  &= ~ PUSH_RIGHT;
    rightPushOptions &= ~ PUSH_LEFT;
    
    [[NSColor blackColor] set];
    NSRect rectToClear = charRect;
    rectToClear.size.width = tileWidth * n;
    NSRectFill(crack_rect(rectToClear, AngbandScaleIdentity, leftPushOptions | rightPushOptions));
    
    NSFont *selectionFont = [[angbandContext selectionFont] screenFont];
    [selectionFont set];
    
    /* Handle overdraws */
    const int overdraws[2] = {x-1, x+n}; //left, right
    for (int i=0; i < 2; i++) {
        int overdrawX = overdraws[i];
        
        // Nothing to overdraw if we're at an edge
        if (overdrawX >= 0 && (size_t)overdrawX < angbandContext->cols)
        {
            wchar_t previouslyDrawnVal = angbandContext->charOverdrawCache[y * angbandContext->cols + overdrawX];
            
            NSRect overdrawRect = [angbandContext rectInImageForTileAtX:overdrawX Y:y];
            NSRect expandedRect = crack_rect(overdrawRect, AngbandScaleIdentity, push_options(overdrawX, y));
            
            [[NSColor blackColor] set];
            NSRectFill(expandedRect);
            redisplayRect = NSUnionRect(redisplayRect, expandedRect);
            
            if (previouslyDrawnVal && previouslyDrawnVal != NO_OVERDRAW)
            {
                byte color = angbandContext->attrOverdrawCache[y * angbandContext->cols + overdrawX];
                
                set_color_for_index(color);
                [angbandContext drawWChar:previouslyDrawnVal inRect:overdrawRect];
            }
        }
    }
    
    set_color_for_index(a);
    
    /* Draw each */
    NSRect rectToDraw = charRect;
    for (int i=0; i < n; i++) {
        [angbandContext drawWChar:cp[i] inRect:rectToDraw];
        rectToDraw.origin.x += tileWidth;
    }
    
    // Invalidate what we just drew
    NSRect drawnRect = charRect;
    drawnRect.size.width = tileWidth * n;
    redisplayRect = NSUnionRect(redisplayRect, drawnRect);
    
    [angbandContext unlockFocus];    
    [angbandContext setNeedsDisplayInBaseRect:redisplayRect];
    
    [pool drain];
    
    return 0;
}

/* Post a nonsense event so that our event loop wakes up */
static void wakeup_event_loop(void)
{
    /* Big hack - send a nonsense event to make us update */
    NSEvent *event = [NSEvent otherEventWithType:NSApplicationDefined location:NSZeroPoint modifierFlags:0 timestamp:0 windowNumber:0 context:NULL subtype:AngbandEventWakeup data1:0 data2:0];
    [NSApp postEvent:event atStart:NO];
}


/*
 * Create and initialize window number "i"
 */
static term *term_data_link(int i)
{
    NSArray *terminalDefaults = [[NSUserDefaults standardUserDefaults] valueForKey: AngbandTerminalsDefaultsKey];
    NSInteger rows = 24;
    NSInteger columns = 80;

    if( i < (int)[terminalDefaults count] )
    {
        NSDictionary *term = [terminalDefaults objectAtIndex: i];
        rows = [[term valueForKey: AngbandTerminalRowsDefaultsKey] integerValue];
        columns = [[term valueForKey: AngbandTerminalColumnsDefaultsKey] integerValue];
    }

    /* Allocate */
    term *newterm = ZNEW(term);

    /* Initialize the term */
    term_init(newterm, MIN(columns, 255), MIN(rows, 255), 256 /* keypresses, for some reason? */);
    
    /* Differentiate between BS/^h, Tab/^i, etc. */
    //newterm->complex_input = TRUE;

    /* Use a "software" cursor */
    newterm->soft_cursor = TRUE;
    
    /* Erase with "white space" */
    newterm->attr_blank = TERM_WHITE;
    newterm->char_blank = ' ';
    
    /* Prepare the init/nuke hooks */
    newterm->init_hook = Term_init_cocoa;
    newterm->nuke_hook = Term_nuke_cocoa;
    
    /* Prepare the function hooks */
    newterm->xtra_hook = Term_xtra_cocoa;
    newterm->wipe_hook = Term_wipe_cocoa;
    newterm->curs_hook = Term_curs_cocoa;
    newterm->text_hook = Term_text_cocoa;
    //newterm->pict_hook = Term_pict_cocoa;
    
    /* Global pointer */
    angband_term[i] = newterm;
    
    return newterm;
}

/*
 * Load preferences from preferences file for current host+current user+
 * current application.
 */
static void load_prefs()
{
    NSUserDefaults *defs = [NSUserDefaults angbandDefaults];
    
    /* Make some default defaults */
    NSMutableArray *defaultTerms = [[NSMutableArray alloc] init];
    NSDictionary *standardTerm = @{
                                   AngbandTerminalRowsDefaultsKey : @24,
                                   AngbandTerminalColumnsDefaultsKey : @80,
                                   };
    
    for( NSUInteger i = 0; i < 9; i++ )
    {
        [defaultTerms addObject: standardTerm];
    }

    NSDictionary *defaults = [[NSDictionary alloc] initWithObjectsAndKeys:
                              @"Menlo", @"FontName",
                              [NSNumber numberWithFloat:14.f], @"FontSize",
                              [NSNumber numberWithInt:60], @"FramesPerSecond",
                              [NSNumber numberWithBool:YES], @"AllowSound",
                              [NSNumber numberWithInt:GRAPHICS_NONE], @"GraphicsID",
                              defaultTerms, AngbandTerminalsDefaultsKey,
                              nil];
    [defs registerDefaults:defaults];
    [defaults release];
    [defaultTerms release];
    
    /* preferred graphics mode */
    graf_mode_req = [defs integerForKey:@"GraphicsID"];
    
    /* use sounds */
    allow_sounds = [defs boolForKey:@"AllowSound"];
    
    /* fps */
    frames_per_second = [[NSUserDefaults angbandDefaults] integerForKey:@"FramesPerSecond"];
    
    /* font */
    default_font = [[NSFont fontWithName:[defs valueForKey:@"FontName-0"] size:[defs floatForKey:@"FontSize-0"]] retain];
    if (! default_font) default_font = [[NSFont fontWithName:@"Menlo" size:14.f] retain];
}

/* Arbitary limit on number of possible samples per event */
#define MAX_SAMPLES            8

/* Struct representing all data for a set of event samples */
typedef struct
{
	int num;        /* Number of available samples for this event */
	NSSound *sound[MAX_SAMPLES];
} sound_sample_list;

/* Array of event sound structs */
//static sound_sample_list samples[MSG_MAX];


/*
 * Load sound effects based on sound.cfg within the xtra/sound directory;
 * bridge to Cocoa to use NSSound for simple loading and playback, avoiding
 * I/O latency by cacheing all sounds at the start.  Inherits full sound
 * format support from Quicktime base/plugins.
 * pelpel favoured a plist-based parser for the future but .cfg support
 * improves cross-platform compatibility.
 */
static void load_sounds(void)
{
#if 0
	char path[2048];
	char buffer[2048];
	ang_file *fff;
    
	/* Build the "sound" path */
	path_build(path, sizeof(path), ANGBAND_DIR_XTRA, "sound");
	ANGBAND_DIR_XTRA_SOUND = string_make(path);
    
	/* Find and open the config file */
	path_build(path, sizeof(path), ANGBAND_DIR_XTRA_SOUND, "sound.cfg");
	fff = file_open(path, MODE_READ, -1);
    
	/* Handle errors */
	if (!fff)
	{
		NSLog(@"The sound configuration file could not be opened.");
		return;
	}
	
	/* Instantiate an autorelease pool for use by NSSound */
	NSAutoreleasePool *autorelease_pool;
	autorelease_pool = [[NSAutoreleasePool alloc] init];
    
    /* Use a dictionary to unique sounds, so we can share NSSounds across multiple events */
    NSMutableDictionary *sound_dict = [NSMutableDictionary dictionary];
    
	/*
	 * This loop may take a while depending on the count and size of samples
	 * to load.
	 */
    
	/* Parse the file */
	/* Lines are always of the form "name = sample [sample ...]" */
	while (file_getl(fff, buffer, sizeof(buffer)))
	{
		char *msg_name;
		char *cfg_sample_list;
		char *search;
		char *cur_token;
		char *next_token;
		int event;
        
		/* Skip anything not beginning with an alphabetic character */
		if (!buffer[0] || !isalpha((unsigned char)buffer[0])) continue;
        
		/* Split the line into two: message name, and the rest */
		search = strchr(buffer, ' ');
		cfg_sample_list = strchr(search + 1, ' ');
		if (!search) continue;
		if (!cfg_sample_list) continue;
        
		/* Set the message name, and terminate at first space */
		msg_name = buffer;
		search[0] = '\0';
        
		/* Make sure this is a valid event name */
		for (event = MSG_MAX - 1; event >= 0; event--)
		{
			if (strcmp(msg_name, angband_sound_name[event]) == 0)
				break;
		}
		if (event < 0) continue;
        
		/* Advance the sample list pointer so it's at the beginning of text */
		cfg_sample_list++;
		if (!cfg_sample_list[0]) continue;
        
		/* Terminate the current token */
		cur_token = cfg_sample_list;
		search = strchr(cur_token, ' ');
		if (search)
		{
			search[0] = '\0';
			next_token = search + 1;
		}
		else
		{
			next_token = NULL;
		}
        
		/*
		 * Now we find all the sample names and add them one by one
		 */
		while (cur_token)
		{
			int num = samples[event].num;
            
			/* Don't allow too many samples */
			if (num >= MAX_SAMPLES) break;
            
            NSString *token_string = [NSString stringWithUTF8String:cur_token];
            NSSound *sound = [sound_dict objectForKey:token_string];
            
            if (! sound)
            {
                /* We have to load the sound. Build the path to the sample */
                path_build(path, sizeof(path), ANGBAND_DIR_XTRA_SOUND, cur_token);
                if (file_exists(path))
                {
                    
                    /* Load the sound into memory */
                    sound = [[[NSSound alloc] initWithContentsOfFile:[NSString stringWithUTF8String:path] byReference:YES] autorelease];
                    if (sound) [sound_dict setObject:sound forKey:token_string];
                }
            }
            
            /* Store it if we loaded it */
            if (sound)
            {
                samples[event].sound[num] = [sound retain];
                
                /* Imcrement the sample count */
                samples[event].num++;
            }
            
            
			/* Figure out next token */
			cur_token = next_token;
			if (next_token)
			{
				/* Try to find a space */
				search = strchr(cur_token, ' ');
                
				/* If we can find one, terminate, and set new "next" */
				if (search)
				{
					search[0] = '\0';
					next_token = search + 1;
				}
				else
				{
					/* Otherwise prevent infinite looping */
					next_token = NULL;
				}
			}
		}
	}
    
	/* Release the autorelease pool */
	[autorelease_pool release];
    
	/* Close the file */
	file_close(fff);
#endif
}
#if 0
/*
 * Play sound effects asynchronously.  Select a sound from any available
 * for the required event, and bridge to Cocoa to play it.
 */
static void play_sound(int event)
{    
    /* Maybe block it */
    if (! allow_sounds) return;
    
	/* Paranoia */
	if (event < 0 || event >= MSG_MAX) return;
    
    /* Load sounds just-in-time (once) */
    static BOOL loaded = NO;
    if (! loaded)
    {
        loaded = YES;
        load_sounds();
    }
    
    /* Check there are samples for this event */
    if (!samples[event].num) return;
    
    /* Instantiate an autorelease pool for use by NSSound */
    NSAutoreleasePool *autorelease_pool;
    autorelease_pool = [[NSAutoreleasePool alloc] init];
    
    /* Choose a random event */
    int s = randint0(samples[event].num);
    
    /* Stop the sound if it's currently playing */
    if ([samples[event].sound[s] isPlaying])
        [samples[event].sound[s] stop];
    
    /* Play the sound */
    [samples[event].sound[s] play];
    
    /* Release the autorelease pool */
    [autorelease_pool drain];
}
#endif

static void init_windows(void)
{
    for (int i=0; i < ANGBAND_TERM_MAX; i++) {
        term_data_link(i);
    }
    
    Term_activate(angband_term[0]);
}

/* Return the directory into which we put data (save and config) */
static NSString *get_data_directory(void)
{
    return [@"~/Documents/PosChengband/" stringByExpandingTildeInPath];
}

/*
 * Handle quit_when_ready, by Peter Ammon,
 * slightly modified to check inkey_flag.
 */
static void quit_calmly(void)
{
    /* Quit immediately if game's not started */
    if (!game_in_progress || !character_generated) quit(NULL);
    
    /* Save the game and Quit (if it's safe) */
    if (inkey_flag)
    {
        do_cmd_save_game(FALSE);
        record_current_savefile();
        
        quit(NULL);
    }
    
    /* Wait until inkey_flag is set */
}



/* returns YES if we contain an AngbandView (and hence should direct our events to Angband) */
static BOOL contains_angband_view(NSView *view)
{
    if ([view isKindOfClass:[AngbandView class]]) return YES;
    for (NSView *subview in [view subviews]) {
        if (contains_angband_view(subview)) return YES;
    }
    return NO;
}

/* Encodes an NSEvent Angband-style, or forwards it along.  Returns YES if the event was sent to Angband, NO if Cocoa (or nothing) handled it */
static BOOL send_event(NSEvent *event)
{
    /* If the receiving window is not an Angband window, then do nothing */
    if (! contains_angband_view([[event window] contentView]))
    {
        [NSApp sendEvent:event];
        return NO;
    }
    
    /* Analyze the event */
    switch ([event type])
    {
        case NSKeyDown:
        {
          if ([[NSApp mainMenu] performKeyEquivalent:event]) break;
          
          [NSCursor setHiddenUntilMouseMoves:YES];
          
          unsigned modifiers = [event modifierFlags];
          int mc = !!(modifiers & NSControlKeyMask);
          int ms = !!(modifiers & NSShiftKeyMask);
          int mo = !!(modifiers & NSAlternateKeyMask);
          int mx = !!(modifiers & NSCommandKeyMask);
          //int kp = !!(modifiers & NSNumericPadKeyMask);
          
          unichar c = ([[event characters] length] > 0) ? [[event characters] characterAtIndex:0] : 0;
          char ch = 0;
          
          int vk = [event keyCode]; // Cocoa virtual key code
          
          // -----------------------
          // Step 2: Map Cocoa key codes for known special keys
          // -----------------------
          bool special = false;
          switch (vk) {
            case kVK_ANSI_Keypad0:
            case kVK_ANSI_Keypad1:
            case kVK_ANSI_Keypad2:
            case kVK_ANSI_Keypad3:
            case kVK_ANSI_Keypad4:
            case kVK_ANSI_Keypad5:
            case kVK_ANSI_Keypad6:
            case kVK_ANSI_Keypad7:
            case kVK_ANSI_Keypad8:
            case kVK_ANSI_Keypad9:
            case kVK_UpArrow:
            case kVK_DownArrow:
            case kVK_LeftArrow:
            case kVK_RightArrow:
            case kVK_PageUp:
            case kVK_PageDown:
            case kVK_Home:
            case kVK_End:
            case kVK_Help:
            case kVK_ForwardDelete:
                special = true;
                break;
            
            case kVK_ANSI_KeypadEnter:
            case kVK_Return:  ch = '\r';   break;
            case kVK_Tab:     ch = '\t';   break;
            case kVK_Delete:  ch = '\b';   break;
            case kVK_Escape:  ch = ESCAPE; break;
            default:
                break;
          }
          
          if (ch) {
            Term_keypress(ch);
            break;
          }
          
          // -----------------------
          // Step 3: Encode special keys using macro sequence
          // -----------------------
          if (special && vk >= 64) {
            Term_keypress(31);  // Begin macro sequence
            
            if (mc) Term_keypress('C');
            if (ms) Term_keypress('S');
            if (mo) Term_keypress('O');
            if (mx) Term_keypress('X');
            
            Term_keypress('0' + (vk - 64) / 10);
            Term_keypress('0' + (vk - 64) % 10);
            
            Term_keypress(13); // End macro sequence
            break;
          }
          
          // -----------------------
          // Step 1: Handle basic ASCII (if no modifiers)
          // -----------------------
          if (c <= 0x7F) Term_keypress((char)c); // Fallback: try printable character anyway
          
          break;
        }

        case NSLeftMouseUp:
        case NSRightMouseUp:
        {
          if (mouse_cursor_targeting_state == 1 || 1)
          {
            AngbandContext *angbandContext = [[[event window] contentView] angbandContext];
            AngbandContext *mainAngbandContext = angband_term[0]->data;

            if (mainAngbandContext->primaryWindow && [[event window] windowNumber] == [mainAngbandContext->primaryWindow windowNumber])
            {
              int cols, rows, x, y;
              Term_get_size(&cols, &rows);

              NSPoint p = [event locationInWindow];
              NSSize tileSize = angbandContext->tileSize;
              NSRect contentRect = [[event window] contentRectForFrameRect:[[event window] frame]];

              // Coordinate conversion: Cocoa origin is bottom-left, Angband is top-left
              x = (p.x - angbandContext->borderSize.width) / tileSize.width;
              y = ((contentRect.size.height - p.y) - angbandContext->borderSize.height) / tileSize.height;

              // Clamp to bounds
              mouse_cursor_x = MAX(0, MIN(x, cols - 1));
              mouse_cursor_y = MAX(0, MIN(y, rows - 1));
              mouse_cursor_targeting_state = 2;
              
              if([event type] == NSLeftMouseUp)  Term_keypress('|');
              if([event type] == NSRightMouseUp) Term_keypress('`');
            }
          }
          
          [NSApp sendEvent:event]; // Pass event through so other UI continues to work
          break;
        }
        
        case NSApplicationDefined:
        {
            if ([event subtype] == AngbandEventWakeup)
            {
                return YES;
            }
            break;
        }
            
        default:
            [NSApp sendEvent:event];
            return YES;
            break;
    }
    return YES;
}

/*
 * Check for Events, return TRUE if we process any
 */
static BOOL check_events(int wait)
{ 
    
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    
    /* Handles the quit_when_ready flag */
    if (quit_when_ready) quit_calmly();
    
    NSDate* endDate;
    if (wait == CHECK_EVENTS_WAIT) endDate = [NSDate distantFuture];
    else endDate = [NSDate distantPast];
    
    NSEvent* event;
    for (;;) {
        if (quit_when_ready)
        {
            /* send escape events until we quit */
            Term_keypress(0x1B);
            [pool drain];
            return false;
        }
        else {
            event = [NSApp nextEventMatchingMask:-1 untilDate:endDate inMode:NSDefaultRunLoopMode dequeue:YES];
            if (! event)
            {
                [pool drain];
                return FALSE;
            }
            if (send_event(event)) break;
        }
    }
    
    [pool drain];
    
    /* Something happened */
    return YES;
}

/*
 * Hook to tell the user something important
 */
static void hook_plog(const char * str)
{
    if (str)
    {
        NSString *string = [NSString stringWithCString:str encoding:NSMacOSRomanStringEncoding];
        NSRunAlertPanel(@"Danger Will Robinson", @"%@", @"OK", nil, nil, string);
    }
}


/*
 * Hook to tell the user something, and then quit
 */
static void hook_quit(const char * str)
{
    plog(str);
    exit(0);
}

/*** Main program ***/

@interface AngbandAppDelegate : NSObject {
    IBOutlet NSMenu *terminalsMenu;
    NSMenu *_commandMenu;
    NSDictionary *_commandMenuTagMap;
}

@property (nonatomic, retain) IBOutlet NSMenu *commandMenu;
@property (nonatomic, retain) NSDictionary *commandMenuTagMap;

- (IBAction)newGame:sender;
- (IBAction)editFont:sender;
- (IBAction)openGame:sender;

- (IBAction)selectWindow: (id)sender;
- (IBAction)cycleWindows: (id)sender;
@end

@implementation AngbandAppDelegate

@synthesize commandMenu=_commandMenu;
@synthesize commandMenuTagMap=_commandMenuTagMap;

- (IBAction)newGame:sender
{
    game_in_progress = TRUE;
    new_game = TRUE;
}

- (IBAction)editFont:sender
{
    NSFontPanel *panel = [NSFontPanel sharedFontPanel];
    NSFont *termFont = default_font;
  
    for (int i=0; i < ANGBAND_TERM_MAX; i++) {
        if ([(id)angband_term[i]->data isMainWindow]) {
            termFont = [(id)angband_term[i]->data selectionFont];
            break;
        }
    }
    
    [panel setPanelFont:termFont isMultiple:NO];
    [panel orderFront:self];
}

- (void)changeFont:(id)sender
{
    int mainTerm;
    for (mainTerm=0; mainTerm < ANGBAND_TERM_MAX; mainTerm++) {
        if ([(id)angband_term[mainTerm]->data isMainWindow]) break;
    }
    if (mainTerm >= ANGBAND_TERM_MAX) return; // Bug #1709: Only change font for angband windows
    
    NSFont *oldFont = default_font;
    NSFont *newFont = [sender convertFont:oldFont];
    if (! newFont) return; //paranoia
    
    /* Store as the default font if we changed the first term */
    if (mainTerm == 0) {
        [newFont retain];
        [default_font release];
        default_font = newFont;
    }
    
    /* Record it in the preferences */
    NSUserDefaults *defs = [NSUserDefaults angbandDefaults];
    [defs setValue:[newFont  fontName] forKey:[NSString stringWithFormat:@"FontName-%d", mainTerm]];
    [defs setFloat:[newFont pointSize] forKey:[NSString stringWithFormat:@"FontSize-%d", mainTerm]];
    [defs synchronize];
    
    NSDisableScreenUpdates();
    
    /* Update window */
    AngbandContext *angbandContext = angband_term[mainTerm]->data;
    [(id)angbandContext setSelectionFont:newFont adjustTerminal: YES];
    
    NSEnableScreenUpdates();
}

- (IBAction)openGame:sender
{
    NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
    BOOL selectedSomething = NO;
    int panelResult;
    NSString* startingDirectory;
    
    /* Get where we think the save files are */
    startingDirectory = [get_data_directory() stringByAppendingPathComponent:@"/save/"];
    
    /* Get what we think the default save file name is. Deafult to the empty string. */
    NSString *savefileName = [[NSUserDefaults angbandDefaults] stringForKey:@"SaveFile"];
    if (! savefileName) savefileName = @"";
    
    /* Set up an open panel */
    NSOpenPanel* panel = [NSOpenPanel openPanel];
    [panel setCanChooseFiles:YES];
    [panel setCanChooseDirectories:NO];
    [panel setResolvesAliases:YES];
    [panel setAllowsMultipleSelection:YES];
    [panel setTreatsFilePackagesAsDirectories:YES];
    
    /* Run it */
    panelResult = [panel runModalForDirectory:startingDirectory file:savefileName types:nil];
    if (panelResult == NSOKButton)
    {
        NSArray* filenames = [panel filenames];
        if ([filenames count] > 0)
        {
            selectedSomething = [[filenames objectAtIndex:0] getFileSystemRepresentation:savefile maxLength:sizeof savefile];
        }
    }
    
    if (selectedSomething)
    {
        
        /* Remember this so we can select it by default next time */
        record_current_savefile();
        
        game_in_progress = TRUE;
        new_game = FALSE;
    }
    
    [pool drain];
}

- (IBAction)saveGame:sender
{
    do_cmd_save_game(FALSE);
    
    /* Record the current save file so we can select it by default next time. It's a little sketchy that this only happens when we save through the menu; ideally game-triggered saves would trigger it too. */
    record_current_savefile();
}

- (BOOL)validateMenuItem:(NSMenuItem *)menuItem
{
    SEL sel = [menuItem action];
    NSInteger tag = [menuItem tag];

    if( tag >= AngbandWindowMenuItemTagBase && tag < AngbandWindowMenuItemTagBase + ANGBAND_TERM_MAX )
    {
        if( tag == AngbandWindowMenuItemTagBase )
        {
            return YES; // the main window should always be available and visible
        }
        else
        {
            NSInteger subwindowNumber = tag - AngbandWindowMenuItemTagBase;
            return (window_flag[subwindowNumber] > 0);
        }

        return NO;
    }

    if (sel == @selector(newGame:))
    {
        return ! game_in_progress;
    }
    else if (sel == @selector(editFont:))
    {
        return YES;
    }
    else if (sel == @selector(openGame:))
    {
        return ! game_in_progress;
    }
    else if (sel == @selector(setRefreshRate:) && [superitem(menuItem) tag] == 150)
    {
        NSInteger fps = [[NSUserDefaults standardUserDefaults] integerForKey: @"FramesPerSecond"];
        [menuItem setState: ([menuItem tag] == fps)];
        return YES;
    }
    else if( sel == @selector(setGraphicsMode:) )
    {
        NSInteger requestedGraphicsMode = [[NSUserDefaults standardUserDefaults] integerForKey: @"GraphicsID"];
        [menuItem setState: (tag == requestedGraphicsMode)];
        return YES;
    }
    else if( sel == @selector(sendAngbandCommand:) )
    {
        return !!game_in_progress; // we only want to be able to send commands during an active game
    }
    else return YES;
}


- (IBAction)setRefreshRate:(NSMenuItem *)menuItem
{
    frames_per_second = [menuItem tag];
    [[NSUserDefaults angbandDefaults] setInteger:frames_per_second forKey:@"FramesPerSecond"];
}

- (IBAction)selectWindow: (id)sender
{
    NSInteger subwindowNumber = [(NSMenuItem *)sender tag] - AngbandWindowMenuItemTagBase;
    AngbandContext *context = angband_term[subwindowNumber]->data;
    [context->primaryWindow makeKeyAndOrderFront: self];
}

- (IBAction)cycleWindows:(id)sender
{
  NSWindow *currentKeyWindow = [NSApp keyWindow];
  NSInteger i;
  
  // Find the index of the current key window
  for(i = 0; i < ANGBAND_TERM_MAX; i++) {
    if(angband_term[i] && angband_term[i]->data) {
      AngbandContext *context = angband_term[i]->data;
      if(context->primaryWindow == currentKeyWindow) break;
    }
  }
  
  // Activate the next term window
  for (i++; i <= ANGBAND_TERM_MAX; i++) {
    NSInteger nextIndex = i % ANGBAND_TERM_MAX;
    if(angband_term[nextIndex] && angband_term[nextIndex]->data) {
      AngbandContext *context = angband_term[nextIndex]->data;
      NSWindow *win = context->primaryWindow;
      if([win isVisible]) {
        [win makeKeyAndOrderFront:self];
        return;
      }
    }
  }
}

- (void)prepareWindowsMenu
{
    // get the window menu with default items and add a separator and item for the main window
    NSMenu *windowsMenu = [[NSApplication sharedApplication] windowsMenu];
    [windowsMenu addItem: [NSMenuItem separatorItem]];

    NSMenuItem *angbandItem = [[NSMenuItem alloc] initWithTitle: @"FrogComposband" action: @selector(selectWindow:) keyEquivalent: @"0"];
    [angbandItem setTarget: self];
    [angbandItem setTag: AngbandWindowMenuItemTagBase];
    [windowsMenu addItem: angbandItem];
    [angbandItem release];

    // add items for the additional term windows
    for( NSInteger i = 1; i < ANGBAND_TERM_MAX; i++ )
    {
        NSString *title = [NSString stringWithFormat: @"Term %ld", (long)i];
        NSMenuItem *windowItem = [[NSMenuItem alloc] initWithTitle: title action: @selector(selectWindow:) keyEquivalent: @""];
        [windowItem setTarget: self];
        [windowItem setTag: AngbandWindowMenuItemTagBase + i];
        [windowsMenu addItem: windowItem];
        [windowItem release];
    }
}

- (void)appDidBecomeActive:(NSNotification *)note {
  // Raise utility panels to floating window level when app activates
  for(int i = 1; i < ANGBAND_TERM_MAX; i++) {
    if(angband_term[i] && angband_term[i]->data) {
      AngbandContext *context = angband_term[i]->data;
      [context->primaryWindow setLevel:NSFloatingWindowLevel];
    }
  }
}

- (void)appDidResignActive:(NSNotification *)note {
  // Lower utility panels to normal window level when app deactivates
  for(int i = 1; i < ANGBAND_TERM_MAX; i++) {
    if(angband_term[i] && angband_term[i]->data) {
      AngbandContext *context = angband_term[i]->data;
      [context->primaryWindow setLevel:NSNormalWindowLevel];
    }
  }
}


/**
 *  Send a command to Angband via a menu item. This places the appropriate key down events into the queue
 *  so that it seems like the user pressed them (instead of trying to use the term directly).
 */
- (void)sendAngbandCommand: (id)sender
{
    NSMenuItem *menuItem = (NSMenuItem *)sender;
    NSString *command = [self.commandMenuTagMap objectForKey: @([menuItem tag])];
    NSInteger windowNumber = [((AngbandContext *)angband_term[0]->data)->primaryWindow windowNumber];

    // send a \ to bypass keymaps
    NSEvent *escape = [NSEvent keyEventWithType: NSKeyDown
                                       location: NSZeroPoint
                                  modifierFlags: 0
                                      timestamp: 0.0
                                   windowNumber: windowNumber
                                        context: nil
                                     characters: @"\\"
                    charactersIgnoringModifiers: @"\\"
                                      isARepeat: NO
                                        keyCode: 0];
    [[NSApplication sharedApplication] postEvent: escape atStart: NO];

    // send the actual command (from the original command set)
    NSEvent *keyDown = [NSEvent keyEventWithType: NSKeyDown
                                        location: NSZeroPoint
                                   modifierFlags: 0
                                       timestamp: 0.0
                                    windowNumber: windowNumber
                                         context: nil
                                      characters: command
                     charactersIgnoringModifiers: command
                                       isARepeat: NO
                                         keyCode: 0];
    [[NSApplication sharedApplication] postEvent: keyDown atStart: NO];
}

/**
 *  Set up the command menu dynamically, based on CommandMenu.plist.
 */
- (void)prepareCommandMenu
{
    NSString *commandMenuPath = [[NSBundle mainBundle] pathForResource: @"CommandMenu" ofType: @"plist"];
    NSArray *commandMenuItems = [[NSArray alloc] initWithContentsOfFile: commandMenuPath];
    NSMutableDictionary *angbandCommands = [[NSMutableDictionary alloc] init];
    NSInteger tagOffset = 0;

    for( NSDictionary *item in commandMenuItems )
    {
        BOOL useShiftModifier = [[item valueForKey: @"ShiftModifier"] boolValue];
        BOOL useOptionModifier = [[item valueForKey: @"OptionModifier"] boolValue];
        NSUInteger keyModifiers = NSCommandKeyMask;
        keyModifiers |= (useShiftModifier) ? NSShiftKeyMask : 0;
        keyModifiers |= (useOptionModifier) ? NSAlternateKeyMask : 0;

        NSString *title = [item valueForKey: @"Title"];
        NSString *key = [item valueForKey: @"KeyEquivalent"];
        NSMenuItem *menuItem = [[NSMenuItem alloc] initWithTitle: title action: @selector(sendAngbandCommand:) keyEquivalent: key];
        [menuItem setTarget: self];
        [menuItem setKeyEquivalentModifierMask: keyModifiers];
        [menuItem setTag: AngbandCommandMenuItemTagBase + tagOffset];
        [self.commandMenu addItem: menuItem];
        [menuItem release];

        NSString *angbandCommand = [item valueForKey: @"AngbandCommand"];
        [angbandCommands setObject: angbandCommand forKey: @([menuItem tag])];
        tagOffset++;
    }

    [commandMenuItems release];

    NSDictionary *safeCommands = [[NSDictionary alloc] initWithDictionary: angbandCommands];
    self.commandMenuTagMap = safeCommands;
    [safeCommands release];
    [angbandCommands release];
}

- (void)awakeFromNib
{
    [super awakeFromNib];

    [self prepareWindowsMenu];
    [self prepareCommandMenu];
}

- (void)applicationDidFinishLaunching:sender
{
  [[NSNotificationCenter defaultCenter] addObserver:self
                                           selector:@selector(appDidBecomeActive:)
                                               name:NSApplicationDidBecomeActiveNotification
                                             object:nil];
  
  [[NSNotificationCenter defaultCenter] addObserver:self
                                           selector:@selector(appDidResignActive:)
                                               name:NSApplicationDidResignActiveNotification
                                             object:nil];
  
    [AngbandContext beginGame];
    
    //once beginGame finished, the game is over - that's how Angband works, and we should quit
    game_is_finished = TRUE;
    [NSApp terminate:self];
}

- (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication *)sender
{
    if (p_ptr->playing == FALSE || game_is_finished == TRUE)
    {
        return NSTerminateNow;
    }
    else if (! inkey_flag)
    {
        /* For compatibility with other ports, do not quit in this case */
        return NSTerminateCancel;
    }
    else
    {
        /* Post an escape event so that we can return from our get-key-event function */
        wakeup_event_loop();
        quit_when_ready = true;
        // must return Cancel, not Later, because we need to get out of the run loop and back to Angband's loop
        return NSTerminateCancel;
    }
}

/* Dynamically build the Graphics menu */
- (void)menuNeedsUpdate:(NSMenu *)menu {
    
    /* Only the graphics menu is dynamic */
    //if (! [[menu title] isEqualToString:@"Graphics"])
        return;
#if 0    
    /* If it's non-empty, then we've already built it. Currently graphics modes won't change once created; if they ever can we can remove this check.
       Note that the check mark does change, but that's handled in validateMenuItem: instead of menuNeedsUpdate: */
    if ([menu numberOfItems] > 0) return;
    
    /* This is the action for all these menu items */
    SEL action = @selector(setGraphicsMode:);
    
    /* Add an initial Classic ASCII menu item */
    NSMenuItem *item = [menu addItemWithTitle:@"Classic ASCII" action:action keyEquivalent:@""];
    [item setTag:GRAPHICS_NONE];
    
    /* Walk through the list of graphics modes */
    NSInteger i;
    for (i=0; graphics_modes[i].pNext; i++)
    {
        const graphics_mode *graf = &graphics_modes[i];
        
        /* Make the title. NSMenuItem throws on a nil title, so ensure it's not nil. */
        NSString *title = [[NSString alloc] initWithUTF8String:graf->menuname];
        if (! title) title = [@"(Unknown)" copy];
        
        NSMenuItem *item = [menu addItemWithTitle:title action:action keyEquivalent:@""];
        [item setTag:graf->grafID];
    }
#endif
}

/* Delegate method that gets called if we're asked to open a file. */
- (BOOL)application:(NSApplication *)sender openFiles:(NSArray *)filenames
{
    /* Can't open a file once we've started */
    if (game_in_progress) return NO;
    
    /* We can only open one file. Use the last one. */
    NSString *file = [filenames lastObject];
    if (! file) return NO;
    
    /* Put it in savefile */
    if (! [file getFileSystemRepresentation:savefile maxLength:sizeof savefile]) return NO;
    
    /* Wake us up in case this arrives while we're sitting at the Welcome screen! */
    wakeup_event_loop();
    
    return YES;
}

@end

int main(int argc, char* argv[])
{
    NSApplicationMain(argc, (void*)argv);    
    return (0);
}
