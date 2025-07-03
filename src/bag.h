#ifndef INCLUDED_BAG_H
#define INCLUDED_BAG_H

#include "inv.h"

#define BAG_MAX 26

extern void    bag_init(void);
extern void    bag_ui(void);
extern void    bag_display(doc_ptr doc, obj_p p, int flags);

extern const char* bag_type_name(int sval);
extern void    place_in_bag_ui(void);

/* Adding and removing: bags allow a large number of slots
 * (bag_MAX) but restrict the number arrows, etc. The capacity
 * of the bag may change as the user finds new and better
 * bags in the dungeon. Note: We rely on the cooperation of
 * other code to ensure that the user has equipped a bag. */
extern bool    bag_likes(obj_ptr obj);
extern bool    bag_tolerates(obj_ptr obj);
extern int     bag_capacity(void);
extern int     bag_weight_capacity(void);
extern void    bag_carry(obj_ptr obj); /* combines bag, then carries pack, then overflows */
extern void    bag_remove(slot_t slot);
extern void    bag_remove_all(void); /* player lost bag due to shapeshifting ... */
extern void    bag_drop(obj_ptr obj);

/* Accessing, Iterating, Searching */
extern obj_ptr bag_obj(slot_t slot);
extern int     bag_max(void); /* for (slot = 1; slot <= bag_max(); slot++) ... */

extern inv_ptr bag_filter(obj_p p);
extern void    bag_for_each(obj_f f);
extern void    bag_for_each_that(obj_f f, obj_p p);
extern slot_t  bag_find_first(obj_p p);
extern slot_t  bag_find_next(obj_p p, slot_t prev_match);
extern slot_t  bag_find_art(int which);
extern slot_t  bag_find_ego(int which);
extern slot_t  bag_find_obj(int tval, int sval);
extern slot_t  bag_random_slot(obj_p p);

/* The bag will 'optimize' upon request, combining objects via
 * stacking and resorting. See PN_REORDER and PN_COMBINE, which
 * I've combined into a single method since it is unclear why 
 * they need to be separate. */
extern bool    bag_optimize(void);
extern void    bag_delayed_describe(void);

/* Properties of the Entire Inventory */
extern int     bag_weight_total(obj_p p);
extern int     bag_weight(obj_p p);
extern int     bag_count(obj_p p);
extern int     bag_count_slots(obj_p p);

/* Savefiles */
extern void    bag_load(savefile_ptr file);
extern void    bag_save(savefile_ptr file);
#endif
