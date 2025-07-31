#include "angband.h"

#include <assert.h>

static inv_ptr _inv = NULL;

void bag_init(void)
{
    inv_free(_inv);
    _inv = inv_alloc("Bag", INV_BAG, BAG_MAX);
}

void bag_ui(void)
{
    slot_t slot = equip_find_obj(TV_BAG, SV_ANY);
    if(!slot) {
        msg_print("You don't have a bag equipped.");
        return;
    }
    if(bag_count_slots(NULL) <= 0) {
        msg_format("Your %s is empty.", bag_type_name(equip_obj(slot)->sval));
        return;
    }
    gear_ui(INV_BAG);
}

void bag_display(doc_ptr doc, obj_p p, int flags)
{
    inv_display(_inv, 1, bag_max(), p, doc, flags);
}

const char* bag_type_name(int sval)
{
  if(!sval) {
    slot_t slot = equip_find_obj(TV_BAG, SV_ANY);
    if(slot) sval = equip_obj(slot)->sval;
  }
  
  switch (sval)
  {
    case SV_BAG_SCROLL_CASE:  return "scroll case";
    case SV_BAG_POTION_BELT:  return "potion belt";
    case SV_BAG_DEVICE_CASE:  return "device case";
    case SV_BAG_BOOK_BAG:     return "book bag";
    default:                  return "bag";
  }
}

/* Adding and removing: Bags allow a large number of slots
 * (BAG_MAX) but restrict the number potions, scrolls, etc.
 * The capacity of the bag may change as the user finds
 * new and better bags in the dungeon.
 * Only ONE bag may be equipped at a time! */
bool bag_likes(obj_ptr obj)
{
  if (!equip_find_obj(TV_BAG, SV_ANY)) return FALSE;
  if (obj_is_ammo(obj)) return FALSE; // Ammo should only go into quivers, not bags
  
  if (inv_can_combine(_inv, obj)) return TRUE;
  if (!obj_is_identified(obj)) return FALSE;
  
  if (equip_find_obj(TV_BAG, SV_BAG_POTION_BELT) && obj_is_potion(obj)) return TRUE;
  if (equip_find_obj(TV_BAG, SV_BAG_SCROLL_CASE) && obj_is_scroll(obj)) return TRUE;
  return FALSE;
}

bool bag_tolerates(obj_ptr obj)
{
    if(!equip_find_obj(TV_BAG, SV_ANY)) return FALSE; // No bag equipped
    if(obj_is_ammo(obj)) return FALSE; // Ammo goes in quivers; reject ammo from bags
    //if(obj_is_bag(obj)) return FALSE; // If we want to reject "bags within bags"

    if(obj->weight > (bag_weight_capacity() - bag_weight(NULL))) return FALSE; // No room for this item

    if(equip_find_obj(TV_BAG, SV_BAG_BOOK_BAG))    return (obj_is_book(obj) || obj_is_parchment(obj));
    if(equip_find_obj(TV_BAG, SV_BAG_SCROLL_CASE)) return (obj_is_scroll(obj) || obj_is_parchment(obj));
    if(equip_find_obj(TV_BAG, SV_BAG_POTION_BELT)) return (obj_is_potion(obj) || obj_is_food(obj));

    return TRUE;
}

int bag_capacity(void)
{
    slot_t slot = equip_find_obj(TV_BAG, SV_ANY);
    return slot ? equip_obj(slot)->xtra4 : 0;
}

int bag_weight_capacity(void)
{
  slot_t slot = equip_find_obj(TV_BAG, SV_ANY);
  return slot ? equip_obj(slot)->xtra5 : 0;
}

void bag_carry(obj_ptr obj)
{
    /* Helper for pack_carry and equip_wield */
    int ct  = bag_count(NULL);
    int cap = bag_capacity();
    if (ct >= cap) return;
  
    int wt     = bag_weight(NULL);
    int wt_cap = bag_weight_capacity();
    if (wt >= wt_cap) return;

    int xtra = 0; // Amount that exceeds our capacity
    if (ct + obj->number > cap)
    {
        xtra = ct + obj->number - cap;
        obj->number -= xtra;
    }

    int excess_weight = wt + obj->number * obj->weight - wt_cap;
    if (excess_weight > 0)
    {
        int xtra_by_weight = (excess_weight + obj->weight-1) / obj->weight;
        xtra += xtra_by_weight;
        obj->number -= xtra_by_weight;
    }

    if (obj->number > 0)
    {
        object_mitze(obj, MITZE_PICKUP);
        inv_combine_ex(_inv, obj);

        slot_t slot = inv_add(_inv, obj);
        if (slot)
        {
            obj_ptr new_obj = inv_obj(_inv, slot);
            new_obj->marked |= OM_TOUCHED;
            new_obj->marked &= ~OM_WORN;
            autopick_alter_obj(new_obj, FALSE);
            p_ptr->notice |= PN_OPTIMIZE_BAG;
        }
    }
    obj->number += xtra;
    p_ptr->update |= PU_BONUS; /* must check speed */
    p_ptr->window |= PW_EQUIP; /* a Bag [32 of 110] */
    p_ptr->notice |= PN_CARRY;
}

bool _bag_can_remove(slot_t item_slot) {
  obj_ptr obj = bag_obj(item_slot);
  if (!obj) return FALSE;
  
  slot_t bag_slot = equip_find_obj(TV_BAG, SV_ANY);
  if (!bag_slot) return FALSE;
  
  obj_ptr obj_bag = equip_obj(bag_slot);
  if (!obj_bag) return FALSE;
  
  if(obj_bag->curse_flags & OFC_DEVOURING) {
    char bag_name[MAX_NLEN];
    object_desc(bag_name, obj_bag, OD_COLOR_CODED | OD_NAME_ONLY | OD_OMIT_PREFIX);
    char obj_name[MAX_NLEN];
    object_desc(obj_name, obj, OD_COLOR_CODED | OD_NAME_ONLY | OD_OMIT_PREFIX | OD_SINGULAR);
    
    msg_format("Your %s refuses to release the %s!", bag_name, obj_name);
    obj_learn_curse(obj_bag, OFC_DEVOURING);
    disturb(0, 0);
    return FALSE;
  }

  return TRUE;
}

void bag_remove(slot_t slot)
{
    inv_remove(_inv, slot);
}

void bag_remove_all(void)
{
    for (slot_t slot = 1; slot <= BAG_MAX; slot++)
    {
        obj_ptr obj = bag_obj(slot);

        if (!obj) continue;
        pack_carry_aux(obj);
        obj_release(obj, OBJ_RELEASE_QUIET);
    }
}

void bag_drop(obj_ptr obj)
{
    assert(obj);
    assert(obj->loc.where == INV_BAG);
    assert(obj->number > 0);
    if(!_bag_can_remove(obj->loc.slot)) return;

    int amt = obj->number;

    if (amt > 1)
    {
        amt = get_quantity(NULL, amt);
        if (amt < 1)
        {
            energy_use = 0;
            return;
        }
    }

    obj_drop(obj, amt);
}

/* Accessing, Iterating, Searching */
obj_ptr bag_obj(slot_t slot)
{
    return inv_obj(_inv, slot);
}

int bag_max(void)
{
    return BAG_MAX;
}

inv_ptr bag_filter(obj_p p)
{
    return inv_filter(_inv, p);
}

void bag_for_each(obj_f f)
{
    inv_for_each(_inv, f);
}

void bag_for_each_that(obj_f f, obj_p p)
{
    inv_for_each_that(_inv, f, p);
}

slot_t bag_find_first(obj_p p)
{
    return inv_first(_inv, p);
}

slot_t bag_find_next(obj_p p, slot_t prev_match)
{
    return inv_next(_inv, p, prev_match);
}

slot_t bag_find_art(int which)
{
    return inv_find_art(_inv, which);
}

slot_t bag_find_ego(int which)
{
    return inv_find_ego(_inv, which);
}

slot_t bag_find_obj(int tval, int sval)
{
    return inv_find_obj(_inv, tval, sval);
}

slot_t bag_random_slot(obj_p p)
{
    return inv_random_slot(_inv, p);
}

bool bag_optimize(void)
{
    return inv_optimize(_inv);
}

void bag_delayed_describe(void)
{
    bag_for_each(obj_delayed_describe);
}

/* Properties of the entire inventory */
int bag_weight_total(obj_p p)
{
    slot_t slot = equip_find_obj(TV_BAG, SV_ANY);
    if (!slot) return 0;
    obj_ptr bag = equip_obj(slot);

    float scale = 1.0f;
    switch (bag->sval)
    {
      case SV_BAG_SCROLL_CASE:  scale = 0.5f; break;
      case SV_BAG_POTION_BELT:  scale = 0.5f; break;
      case SV_BAG_BOOK_BAG:     scale = 0.5f; break;
    }

    if(obj_has_flag(bag, OF_ETHEREAL)) scale *= 0.5f;
    if(obj_has_flag(bag, OF_BULKY) || (bag->curse_flags & OFC_BULKY)) scale *= 2.0f;

    return (int)(inv_weight(_inv, p) * scale + 0.5f);
}

int bag_weight(obj_p p)
{
  return inv_weight(_inv, p);
}

int bag_count(obj_p p)
{
    return inv_count(_inv, p);
}

int bag_count_slots(obj_p p)
{
    return inv_count_slots(_inv, p);
}

/* Placing items into bags */
static bool _can_store_in_bag(obj_ptr obj)
{
  if (!obj) return FALSE;
  
  if (obj_is_bag(obj) && obj->loc.where == INV_EQUIP) return FALSE; // Don't try to store a bag inside itself
  return bag_tolerates(obj);
}

static obj_ptr _bag_place_get_obj(void)
{
  obj_ptr bag_obj = equip_obj(equip_find_obj(TV_BAG, SV_ANY));
  
  char bag_name[MAX_NLEN];
  object_desc(bag_name, bag_obj, OD_COLOR_CODED);
  string_ptr s = string_alloc_format("<color:w>Equipped Bag: %s</color>\n\n"
                                     "Place which item into your %s?",
                                     bag_name, bag_type_name(bag_obj->sval));
  
  obj_prompt_t prompt = {0};
  prompt.prompt = string_buffer(s);
  prompt.error = format("You aren't carrying anything that fits in your %s.", bag_type_name(0));
  prompt.filter = _can_store_in_bag;
  prompt.where[0] = INV_PACK;
  prompt.where[1] = INV_EQUIP;
  prompt.where[2] = INV_FLOOR;
  obj_prompt_add_special_packs(&prompt);
  
  obj_prompt(&prompt);
  string_free(s);
  
  return prompt.obj;
}

void bag_place_ui(void)
{
  if(!equip_find_obj(TV_BAG, SV_ANY))
  {
    msg_print("You don't have a bag equipped.");
    return;
  }
  
  if (bag_count_slots(NULL) >= bag_max() || bag_count(NULL) >= bag_capacity())
  {
    msg_print("Your bag is full.");
    return;
  }
  if (bag_weight(NULL) >= bag_weight_capacity())
  {
    msg_print("Your bag cannot carry any more weight.");
    return;
  }
  
  obj_ptr obj = _bag_place_get_obj();
  
  if (!obj) return;
  
  if(obj_is_bag(obj) && obj->name2 == EGO_BAG_HOLDING) {
    char o_name[MAX_NLEN];
    object_desc(o_name, obj, OD_COLOR_CODED | OD_OMIT_PREFIX | OD_NAME_ONLY);
    msg_format("You think for a moment, then decide against putting your %s inside another bag.", o_name);
    return;
  }
  
  int amt = MIN(obj->number, bag_capacity() - bag_count(NULL));
  if (amt == 1 || msg_input_num("Quantity", &amt, 1, amt))
  {
    obj_t copy = *obj;
    copy.number = amt;  // requested number to transfer
    bag_carry(&copy);   // remaining copy.number is excess that wouldn't fit
    amt -= copy.number; // amt is the number actually transferred
    obj->number -= amt; // remaining objects that didn't transfer
    
    if(amt) // we actually transferred something
    {
      obj_release(obj, obj->number ? OBJ_RELEASE_DELAYED_MSG : OBJ_RELEASE_QUIET);
      
      p_ptr->notice |= PN_OPTIMIZE_PACK | PN_OPTIMIZE_BAG;
      p_ptr->window |= PW_INVEN | PW_EQUIP;
      
      energy_use = 50;
    }
  }
}

/* Savefiles */
void bag_load(savefile_ptr file)
{
    inv_load(_inv, file);
}

void bag_save(savefile_ptr file)
{
    inv_save(_inv, file);
}
