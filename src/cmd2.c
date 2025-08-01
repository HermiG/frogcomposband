/* File: cmd2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Movement commands (part 2) */

#include "angband.h"

#include <assert.h>

static bool do_cmd_bash_aux(int y, int x, int dir);

/*
 * Go up one level
 */
void do_cmd_go_up(void)
{
    cave_type *c_ptr = &cave[py][px];
    feature_type *f_ptr = &f_info[c_ptr->feat];

    int up_num = 0;

    if (p_ptr->special_defense & KATA_MUSOU) set_action(ACTION_NONE);

    if (!have_flag(f_ptr->flags, FF_LESS))
    {
        msg_print("I see no up staircase here.");
        return;
    }

    if (!quests_check_leave()) return;

    /* Hack -- take a turn */
    energy_use = 100;
    if (py_in_dungeon())
    {
        /* Mega-hack - avoid skipping midnights */
        if (ironman_nightmare)
        {
            /* Check if midnight happens within 26 game turns */
            int new_day, prev_day, prev_hour, prev_min;
            extract_day_hour_min_imp(game_turn, &prev_day, &prev_hour, &prev_min);
            extract_day_hour_min_imp(game_turn + 26, &new_day, &prev_hour, &prev_min);
            if (new_day != prev_day)
            {
                msg_print("Horrors of the night block the staircase, driving you away!");
                teleport_player(10, 0L);
                return;
            }
        }
        advance_time_hack = TRUE;
    }

    if (autosave_l) do_cmd_save_game(TRUE);

    quests_on_leave();
    if (dun_level)
    {
        if (have_flag(f_ptr->flags, FF_SHAFT))
        {
            /* Create a way back */
            prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_UP | CFM_SHAFT);
            up_num = 2;
        }
        else
        {
            /* Create a way back */
            prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_UP);
            up_num = 1;
        }

        /* Get out from current dungeon */
        if (dun_level - up_num < d_info[dungeon_type].mindepth) up_num = dun_level;

        if (d_info[dungeon_type].flags1 & DF1_RANDOM)
        {
            up_num = dun_level;
            prepare_change_floor_mode(CFM_NO_RETURN);
        }
    }

    if (up_num == dun_level) msg_print("You go back to the surface.");
    else                     msg_print("You enter a maze of up staircases.");

    p_ptr->leaving = TRUE;
}


/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
    /* Player grid */
    cave_type *c_ptr = &cave[py][px];
    feature_type *f_ptr = &f_info[c_ptr->feat];

    bool fall_trap = FALSE;
    int down_num = 0;

    if (p_ptr->special_defense & KATA_MUSOU) set_action(ACTION_NONE);

    /* Verify stairs */
    if (!have_flag(f_ptr->flags, FF_MORE))
    {
        msg_print("I see no down staircase here.");
        return;
    }

    if ((quest_id_current()) && !quests_check_leave()) return;

    if (have_flag(f_ptr->flags, FF_TRAP)) fall_trap = TRUE;

    /* Quest entrance */
    if (have_flag(f_ptr->flags, FF_QUEST_ENTER)) do_cmd_quest();
    else
    {
        int target_dungeon = 0;

        if (!dun_level)
        {
            target_dungeon = have_flag(f_ptr->flags, FF_ENTRANCE) ? c_ptr->special : DUNGEON_ANGBAND;

            if (only_downward() && (target_dungeon != DUNGEON_ANGBAND))
            {
                msg_print("The entrance of this dungeon is closed!");
                return;
            }
            if (!max_dlv[target_dungeon])
            {
                if (d_info[target_dungeon].flags1 & DF1_RANDOM)
                    msg_format("This is the entrance of %s (Danger level: ?)", d_name+d_info[target_dungeon].name);
                else
                    msg_format("This is the entrance of %s (Danger level: %d)", d_name+d_info[target_dungeon].name, d_info[target_dungeon].mindepth);
                if (!get_check("Do you really go into this dungeon? ")) return;
            }

            /* Save old player position */
            p_ptr->oldpx = px;
            p_ptr->oldpy = py;
            set_dungeon_type((byte)target_dungeon);

            /*
             * Clear all saved floors
             * and create a first saved floor
             */
            prepare_change_floor_mode(CFM_FIRST_FLOOR);
        }

        energy_use = 100;

        if (autosave_l) do_cmd_save_game(TRUE);

        quests_on_leave(); /* QS_COMPLETED and taking the victory stairs? */

        /* Go down */
        if (have_flag(f_ptr->flags, FF_SHAFT) && quests_allow_downshaft()) down_num += 2;
        else down_num += 1;

        if (!dun_level)
        {
            /* Enter the dungeon just now */
            p_ptr->enter_dungeon = TRUE;
            down_num = d_info[dungeon_type].mindepth;
        }

        if (fall_trap) msg_print("You deliberately jump through the trap door.");
        else
        {
            if (target_dungeon) msg_format("You entered %s.", d_text + d_info[dungeon_type].text);
            else                msg_print("You enter a maze of down staircases.");
        }

        p_ptr->leaving = TRUE;

        if (fall_trap)
        {
            prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_DOWN | CFM_RAND_PLACE | CFM_RAND_CONNECT);
        }
        else
        {
            /* XXX It is CFM_SHAFT, not down_num, that determines that we descend by 2 */
            if (have_flag(f_ptr->flags, FF_SHAFT) && quests_allow_downshaft())
            {
                /* Create a way back */
                prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_DOWN | CFM_SHAFT);
            }
            else
            {
                /* Create a way back ... maybe */
                if ( p_ptr->enter_dungeon
                  && down_num >= 20
                  && !wacky_rooms
                  && !(d_info[dungeon_type].flags1 & DF1_RANDOM)
                  && !(d_info[dungeon_type].initial_guardian && !(dungeon_flags[dungeon_type] & DUNGEON_NO_GUARDIAN))
                  && one_in_(14) )
                {
                    /* Hack:  No stair scum */
                    msg_print("The stairs collapse behind you! You are trapped!!");
                    dungeon_flags[target_dungeon] |= DUNGEON_NO_ENTRANCE;
                    prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_DOWN | CFM_RAND_PLACE);
                }
                else prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_DOWN);
            }
        }
    }
}



/*
 * Simple command to "search" for one turn
 */
void do_cmd_search(void)
{
    /* Allow repeated command */
    if (command_arg)
    {
        command_rep = command_arg - 1; // Set repeat count
        p_ptr->redraw |= PR_STATE;
        command_arg = 0; // Cancel the arg
    }

    energy_use = 100;
    search();
}

#define _CHEST_CLOSED 0x01
#define _CHEST_TRAPPED 0x02

/*
 * Determine if a grid contains a chest
 */
static s16b chest_check(int y, int x, int mode)
{
    cave_type *c_ptr = &cave[y][x];

    /* Scan all objects in the grid */
    for (s16b this_o_idx = c_ptr->o_idx, next_o_idx = 0; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr = &o_list[this_o_idx];
        next_o_idx = o_ptr->next_o_idx;

        /* Skip unknown chests XXX XXX */
        /* if (!(o_ptr->marked & OM_FOUND)) continue; */

        /* Check for chest */
        if (o_ptr->tval == TV_CHEST)
        {
            if ((mode & _CHEST_TRAPPED) && (!object_is_known(o_ptr) || o_ptr->pval < 0 || !chest_traps[o_ptr->pval])) continue;
            if ((mode & _CHEST_CLOSED) && (!o_ptr->pval)) continue;
            return this_o_idx;
        }
    }

    return 0; // No chest
}


/*
 * Allocates objects upon opening a chest    -BEN-
 *
 * Disperse treasures from the given chest, centered at (x,y).
 *
 * Chests are now a good means of getting gold  -CHRIS-
 *
 */
static bool _chest_scatter(object_type *o_ptr)
{
    int i;
    for (i = 0; i < 200; i++)
    {
        int y = randint0(MAX_HGT);
        int x = randint0(MAX_WID);

        if (!cave_empty_bold(y, x)) continue;
        drop_near(o_ptr, -1, y, x);
        return TRUE;
    }
    return FALSE;
}
static void chest_death(bool scatter, int y, int x, s16b o_idx)
{
    int         ct_objects = 0, ct_gold = 0;
    int         i;
    u32b        mode = AM_GOOD;
    object_type *chest_ptr = &o_list[o_idx];

    if (chest_ptr->sval == SV_CHEST_KANDUME) /* Can of Toys */
    {
        ct_objects = 5;
        ct_gold = 0;
        mode |= AM_GREAT;
        object_level = chest_ptr->xtra3;
    }
    else
    {        /* v~~~~~~~~~ You'll need to look a k_info.txt to understand this ... */
        int num = chest_ptr->sval % SV_CHEST_MIN_LARGE;

        /* object_level = ABS(chest_ptr->pval) + 10;
         * i.e., 1dL+10. Finding an OL99 chest and getting L22 objects is just
         * plain insulting. Chests should be like ?Acquirement, only AM_GOOD rather
         * than AM_GREAT. */
        object_level = chest_ptr->xtra3;
        if (chest_ptr->sval < SV_CHEST_MIN_LARGE)
        {
            ct_gold = damroll(2, num);
            ct_objects = damroll(1, num);
        }
        else
        {
            ct_gold = damroll(3, num);
            ct_objects = damroll(2, num);
        }
    }

    /* Zero pval means empty chest */
    if (!chest_ptr->pval)
    {
        ct_gold = 0;
        ct_objects = 0;
    }

    /* Opening a chest */
    opening_chest = TRUE; /* <==== This hack prevents getting chests from inside chests! */

    for (i = 0; i < ct_objects; i++)
    {
        object_type forge = {0};
        if (!make_object(&forge, mode, ((mode & AM_GREAT) ? ORIGIN_CAN_OF_TOYS : ORIGIN_CHEST))) continue;
        if (scatter) _chest_scatter(&forge);
        else drop_near(&forge, -1, y, x);
    }

    for (i = 0; i < ct_gold; i++)
    {
        object_type forge = {0};
        if (!make_gold(&forge, TRUE)) continue;
        if (scatter) _chest_scatter(&forge);
        else drop_near(&forge, -1, y, x);
    }

    /* Reset the object level */
    object_level = base_level;

    /* No longer opening a chest */
    opening_chest = FALSE;

    /* Empty */
    chest_ptr->pval = 0;

    /* Known */
    obj_identify(chest_ptr);
}


/*
 * Chests have traps too.
 *
 * Exploding chest destroys contents (and traps).
 * Note that the chest itself is never destroyed.
 */
static void chest_trap(int y, int x, s16b o_idx)
{
    int  i, trap;

    object_type *o_ptr = &o_list[o_idx];

    int mon_level = o_ptr->xtra3;

    /* Ignore disarmed chests */
    if (o_ptr->pval <= 0) return;

    /* Obtain the traps */
    trap = chest_traps[o_ptr->pval];

    /* Lose strength */
    if (trap & (CHEST_LOSE_STR))
    {
        msg_print("A small needle has pricked you!");
        take_hit(DAMAGE_NOESCAPE, damroll(1, 4), "a poison needle");

        (void)do_dec_stat(A_STR);
    }

    /* Lose constitution */
    if (trap & (CHEST_LOSE_CON))
    {
        msg_print("A small needle has pricked you!");
        take_hit(DAMAGE_NOESCAPE, damroll(1, 4), "a poison needle");

        (void)do_dec_stat(A_CON);
    }

    /* Poison */
    if (trap & (CHEST_POISON))
    {
        msg_print("A puff of green gas surrounds you!");
        if (!res_save_default(RES_POIS))
            (void)set_poisoned(p_ptr->poisoned + 10 + randint1(20), FALSE);
    }

    /* Paralyze */
    if (trap & (CHEST_PARALYZE))
    {
        msg_print("A puff of yellow gas surrounds you!");
        if (!free_act_save_p(0))
            set_paralyzed(randint1(4), FALSE);
    }

    /* Summon monsters */
    if (trap & (CHEST_SUMMON))
    {
        int num = 2 + randint1(3);
        msg_print("You are enveloped in a cloud of smoke!");


        for (i = 0; i < num; i++)
        {
            if (randint1(100)<dun_level)
                activate_hi_summon(py, px, FALSE);
            else
                (void)summon_specific(0, y, x, mon_level, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
        }
    }

    /* Elemental summon. */
    if (trap & (CHEST_E_SUMMON))
    {
        msg_print("Elemental beings appear to protect their treasures!");
        for (i = 0; i < randint1(3) + 5; i++)
        {
            (void)summon_specific(0, y, x, mon_level, SUMMON_ELEMENTAL, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
        }
    }

    /* Force clouds, then summon birds. */
    if (trap & (CHEST_BIRD_STORM))
    {
        msg_print("A storm of birds swirls around you!");

        for (i = 0; i < randint1(3) + 3; i++)
            (void)fire_meteor(-1, GF_FORCE, y, x, o_ptr->pval / 5, 7);

        for (i = 0; i < randint1(5) + o_ptr->pval / 5; i++)
        {
            (void)summon_specific(0, y, x, mon_level, SUMMON_BIRD, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
        }
    }

    /* Various colorful summonings. */
    if (trap & (CHEST_H_SUMMON))
    {
        /* Summon demons. */
        if (one_in_(4))
        {
            msg_print("Demons materialize in clouds of fire and brimstone!");

            for (i = 0; i < randint1(3) + 2; i++)
            {
                (void)fire_meteor(-1, GF_FIRE, y, x, 10, 5);
                (void)summon_specific(0, y, x, mon_level, SUMMON_DEMON, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
            }
        }

        /* Summon dragons. */
        else if (one_in_(3))
        {
            msg_print("Draconic forms loom out of the darkness!");

            for (i = 0; i < randint1(3) + 2; i++)
            {
                (void)summon_specific(0, y, x, mon_level, SUMMON_DRAGON, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
            }
        }

        /* Summon hybrids. */
        else if (one_in_(2))
        {
            msg_print("Creatures strange and twisted assault you!");

            for (i = 0; i < randint1(5) + 3; i++)
            {
                (void)summon_specific(0, y, x, mon_level, SUMMON_HYBRID, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
            }
        }

        /* Summon vortices (scattered) */
        else
        {
            msg_print("Vortices coalesce and wreak destruction!");

            for (i = 0; i < randint1(3) + 2; i++)
            {
                (void)summon_specific(0, y, x, mon_level, SUMMON_VORTEX, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
            }
        }
    }

    /* Dispel player. */
    if ((trap & (CHEST_RUNES_OF_EVIL)) && o_ptr->k_idx)
    {
        /* Determine how many nasty tricks can be played. */
        int nasty_tricks_count = 4 + randint0(3);

        /* Message. */
        msg_print("Hideous voices bid:  'Let the darkness have thee!'");

        /* This is gonna hurt... */
        for (; nasty_tricks_count > 0; nasty_tricks_count--)
        {
            /* ...but a high saving throw does help a little. */
            if (randint1(100+o_ptr->pval*2) > p_ptr->skills.sav)
            {
                if (one_in_(6)) take_hit(DAMAGE_NOESCAPE, damroll(5, 20), "a chest dispel-player trap");
                else if (one_in_(5)) (void)set_cut(p_ptr->cut + 200, FALSE);
                else if (one_in_(4))
                {
                    if (!free_act_save_p(0))
                        set_paralyzed(randint1(4), FALSE);
                    else
                        set_stun(p_ptr->stun + 10 + randint0(100), FALSE);
                }
                else if (one_in_(3)) apply_disenchant(0);
                else if (one_in_(2))
                {
                    (void)do_dec_stat(A_STR);
                    (void)do_dec_stat(A_DEX);
                    (void)do_dec_stat(A_CON);
                    (void)do_dec_stat(A_INT);
                    (void)do_dec_stat(A_WIS);
                    (void)do_dec_stat(A_CHR);
                }
                else (void)fire_meteor(-1, GF_NETHER, y, x, 150, 1);
            }
        }
    }

    /* Aggravate monsters. */
    if (trap & (CHEST_ALARM))
    {
        msg_print("An alarm sounds!");
        aggravate_monsters(0);
    }

    /* Explode */
    if ((trap & (CHEST_EXPLODE)) && o_ptr->k_idx)
    {
        msg_print("There is a sudden explosion!");
        msg_print("Everything inside the chest is destroyed!");

        o_ptr->pval = 0;
        sound(SOUND_EXPLODE);
        take_hit(DAMAGE_ATTACK, damroll(5, 8), "an exploding chest");

    }
    /* Scatter contents. */
    if ((trap & (CHEST_SCATTER)) && o_ptr->k_idx)
    {
        msg_print("The contents of the chest scatter all over the dungeon!");
        chest_death(TRUE, y, x, o_idx);
        o_ptr->pval = 0;
    }
}


/*
 * Attempt to open the given chest at the given location
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_open_chest(int y, int x, s16b o_idx)
{
    bool open = TRUE;
    bool more = FALSE;

    object_type *o_ptr = &o_list[o_idx];

    /* Take a turn */
    energy_use = 100;

    /* Attempt to unlock it */
    if (o_ptr->pval > 0)
    {
        open = FALSE;
        int skill = p_ptr->skills.dis;
        if (p_ptr->blind    || no_lite())    skill /= 10;
        if (p_ptr->confused || p_ptr->image) skill /= 10;

        int dc = skill - o_ptr->pval;
        if (dc < 2) dc = 2; // Minimum 2% chance to succeed

        if (randint0(100) < dc)
        {
            cmsg_print(TERM_L_UMBER, "You have picked the lock.");
            gain_exp(1);
            open = TRUE;
        }
        else
        {
            more = TRUE; // We may continue lockpicking
            if (flush_failure) flush();
            cmsg_print(TERM_L_UMBER, "You failed to pick the lock.");
        }
    }

    if (open)
    {
        chest_trap(y, x, o_idx); // Activate any chest traps
        chest_death(FALSE, y, x, o_idx); // Drop items from the chest
    }

    water_mana_action(FALSE, 10);

    return more;
}


#if defined(ALLOW_EASY_OPEN) || defined(ALLOW_EASY_DISARM) /* TNB */

/*
 * Return TRUE if the given feature is an open door
 */
static bool is_open(int feat)
{
    return have_flag(f_info[feat].flags, FF_CLOSE) && (feat != feat_state(feat, FF_CLOSE));
}


/*
 * Return the number of features around (or under) the character.
 * Usually look for doors and floor traps.
 */
static int count_dt(int *y, int *x, bool (*test)(int feat), bool under)
{
    int count = 0;

    /* Check around (and perhaps under) the character */
    for (int d = 0; d < 8 + !!under; d++)
    {
        /* Extract adjacent (legal) location */
        int yy = py + ddy_ddd[d];
        int xx = px + ddx_ddd[d];
        cave_type *c_ptr = &cave[yy][xx];

        /* Must have knowledge */
        if (!(c_ptr->info & (CAVE_MARK))) continue;

        /* Feature code (applying "mimic" field) */
        s16b feat = get_feat_mimic(c_ptr);

        /* Not looking for this feature */
        if (!((*test)(feat))) continue;

        count++;

        /* Remember the location. Only useful if only one match */
        *y = yy;
        *x = xx;
    }

    return count;
}


/*
 * Return the number of chests around (or under) the character.
 * If requested, count only trapped chests.
 */
static int count_chests(int *y, int *x, int mode)
{
    int count = 0;

    /* Check around (and under) the character */
    for (int d = 0; d < 9; d++)
    {
        /* Extract adjacent (legal) location */
        int yy = py + ddy_ddd[d];
        int xx = px + ddx_ddd[d];

        int o_idx = chest_check(yy, xx, mode);
        if (!o_idx) continue; // No (visible) chest is there
        if (o_list[o_idx].pval == 0) continue; // Already open

        count++;

        /* Remember the location. Only useful if only one match */
        *y = yy;
        *x = xx;
    }

    return count;
}


/*
 * Convert an adjacent location to a direction.
 */
static int coords_to_dir(int y, int x)
{
    int d[3][3] = { {7, 4, 1}, {8, 5, 2}, {9, 6, 3} };
    int dy, dx;

    dy = y - py;
    dx = x - px;

    /* Paranoia */
    if (ABS(dx) > 1 || ABS(dy) > 1) return (0);

    return d[dx + 1][dy + 1];
}

#endif /* defined(ALLOW_EASY_OPEN) || defined(ALLOW_EASY_DISARM) -- TNB */


/*
 * Perform the basic "open" command on doors
 *
 * Assume destination is a closed/locked/jammed door
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_open_aux(int y, int x)
{
    /* Get requested grid */
    cave_type *c_ptr = &cave[y][x];
    feature_type *f_ptr = &f_info[c_ptr->feat];

    bool more = FALSE;

    /* Take a turn */
    energy_use = 100;

    /* Seeing true feature code (ignore mimic) */

    /* Jammed door */
    if (!have_flag(f_ptr->flags, FF_OPEN))
    {
        cmsg_format(TERM_L_UMBER, "The %s appears to be stuck.", f_name + f_info[get_feat_mimic(c_ptr)].name);
        if (travel.run) flush();
    }

    /* Locked door */
    else if (f_ptr->power)
    {
        int skill = p_ptr->skills.dis;

        if (p_ptr->blind || no_lite())       skill /= 10;
        if (p_ptr->confused || p_ptr->image) skill /= 10;

        int dc = f_ptr->power;
        dc = skill - (dc * 4);
        if (dc < 2) dc = 2; // Minimum 2% chance of success

        if (randint0(100) < dc)
        {
            cmsg_print(TERM_L_UMBER, "You have picked the lock.");
            cave_alter_feat(y, x, FF_OPEN);
            sound(SOUND_OPENDOOR);
            gain_exp(1);
        }
        else
        {
            if (flush_failure && !travel.run) flush();
            cmsg_print(TERM_L_UMBER, "You failed to pick the lock.");
            more = TRUE; // We may keep trying
        }
    }
    else // Closed door
    {
        cave_alter_feat(y, x, FF_OPEN);
        sound(SOUND_OPENDOOR);
    }
    water_mana_action(FALSE, 10);

    return more;
}



/*
 * Open a closed/locked/jammed door or a closed/locked chest.
 *
 * Unlocking a locked door/chest is worth one experience point.
 */
void do_cmd_open(void)
{
    int dir;
    bool more = FALSE;

    if (p_ptr->special_defense & KATA_MUSOU) set_action(ACTION_NONE);

#ifdef ALLOW_EASY_OPEN /* TNB */

    /* Option: Pick a direction */
    if (easy_open)
    {
        int y, x;
        int count = count_dt(&y, &x, is_closed_door, FALSE) + count_chests(&y, &x, _CHEST_CLOSED);
        if (count == 1) command_dir = coords_to_dir(y, x);
    }

#endif /* ALLOW_EASY_OPEN -- TNB */

    /* Allow repeated command */
    if (command_arg)
    {
        command_rep = command_arg - 1; // Set repeat count
        p_ptr->redraw |= PR_STATE;
        command_arg = 0; // Cancel the arg
    }

    /* Get a "repeated" direction */
    if (get_rep_dir(&dir, TRUE))
    {
        int y = py + ddy[dir];
        int x = px + ddx[dir];
        cave_type *c_ptr = &cave[y][x];

        /* Feature code (applying "mimic" field) */
        s16b feat = get_feat_mimic(c_ptr);
        s16b o_idx = chest_check(y, x, _CHEST_CLOSED);

        /* Nothing useful */
        if (!have_flag(f_info[feat].flags, FF_OPEN) && !o_idx)
        {
            msg_print("You see nothing there to open.");
            if (p_ptr->confused) energy_use = 50;
        }
        else if (c_ptr->m_idx && p_ptr->riding != c_ptr->m_idx)
        {
            msg_print("There is a monster in the way!");
            energy_use = 100;
            py_attack(y, x, 0);
        }

        /* Handle chests */
        else if (o_idx) more = do_cmd_open_chest(y, x, o_idx);
        else
        {
            if (p_ptr->prace == RACE_MON_VORTEX) more = do_cmd_bash_aux(y, x, dir);
            else                                 more = do_cmd_open_aux(y, x);
        }
    }

    /* Cancel repeat unless we may continue */
    if (!more && !travel.run) disturb(0, 0);
}



/*
 * Perform the basic "close" command
 *
 * Assume destination is an open/broken door
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_close_aux(int y, int x)
{
    /* Get grid and contents */
    cave_type *c_ptr = &cave[y][x];
    s16b      old_feat = c_ptr->feat;

    /* Take a turn */
    energy_use = 100;

    /* Seeing true feature code (ignore mimic) */

    /* Open door */
    if (have_flag(f_info[old_feat].flags, FF_CLOSE))
    {
        s16b closed_feat = feat_state(old_feat, FF_CLOSE);

        /* Hack -- object in the way */
        if ((c_ptr->o_idx || (c_ptr->info & CAVE_OBJECT)) &&
            (closed_feat != old_feat) && !have_flag(f_info[closed_feat].flags, FF_DROP))
        {
            cmsg_print(TERM_L_UMBER, "There's something blocking the door.");
        }
        else
        {
            cave_alter_feat(y, x, FF_CLOSE);
            if (old_feat == c_ptr->feat) cmsg_print(TERM_L_UMBER, "The door appears to be broken.");
            else sound(SOUND_SHUTDOOR);
        }
    }

    return FALSE; // There's no point in trying to close it again
}


/*
 * Close an open door.
 */
void do_cmd_close(void)
{
    int dir;
    bool more = FALSE;

    if (p_ptr->special_defense & KATA_MUSOU) set_action(ACTION_NONE);

#ifdef ALLOW_EASY_OPEN /* TNB */
    /* Option: Pick a direction */
    if (easy_open)
    {
        int y, x;
        if (count_dt(&y, &x, is_open, FALSE) == 1) command_dir = coords_to_dir(y, x);
    }
#endif /* ALLOW_EASY_OPEN -- TNB */

    /* Allow repeated command */
    if (command_arg)
    {
        command_rep = command_arg - 1; // Set repeat count
        p_ptr->redraw |= PR_STATE;
        command_arg = 0; // Cancel the arg
    }

    /* Get a "repeated" direction */
    if (get_rep_dir(&dir,FALSE))
    {
        int y = py + ddy[dir];
        int x = px + ddx[dir];
        cave_type *c_ptr = &cave[y][x];

        /* Feature code (applying "mimic" field) */
        s16b feat = get_feat_mimic(c_ptr);

        /* Require open/broken door */
        if (!have_flag(f_info[feat].flags, FF_CLOSE))
        {
            cmsg_print(TERM_L_UMBER, "You see nothing there to close.");
            if (p_ptr->confused) energy_use = 50; /* prevent free turns until the right direction is picked */
        }
        else if (c_ptr->m_idx)
        {
            energy_use = 100;
            cmsg_print(TERM_L_UMBER, "A monster blocks the doorway!");
            py_attack(y, x, 0);
        }
        else more = do_cmd_close_aux(y, x);
    }

    /* Cancel repeat unless we may continue */
    if (!more) disturb(0, 0);
}


/*
 * Determine if a given grid may be "tunneled"
 */
static bool do_cmd_tunnel_test(int y, int x)
{
    cave_type *c_ptr = &cave[y][x];

    /* Must have knowledge */
    if (!(c_ptr->info & CAVE_MARK))
    {
        msg_print("You see nothing there.");
        return FALSE;
    }

    /* Must be a wall/door/etc */
    if (!cave_have_flag_grid(c_ptr, FF_TUNNEL))
    {
        msg_print("You see nothing there to tunnel through.");
        return FALSE;
    }

    return TRUE; // Can repeat
}


/*
 * Perform the basic "tunnel" command
 *
 * Assumes that no monster is blocking the destination
 *
 * Do not use twall anymore
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_tunnel_aux(int y, int x)
{
    cave_type *c_ptr;
    feature_type *f_ptr, *mimic_f_ptr;
    int power;
    cptr name;
    bool more = FALSE;

    /* Verify legality */
    if (!do_cmd_tunnel_test(y, x)) return (FALSE);

    /* Take a turn */
    energy_use = 100;

    /* Get grid */
    c_ptr = &cave[y][x];
    f_ptr = &f_info[c_ptr->feat];
    power = f_ptr->power;

    /* Feature code (applying "mimic" field) */
    mimic_f_ptr = &f_info[get_feat_mimic(c_ptr)];

    name = f_name + mimic_f_ptr->name;

    sound(SOUND_DIG);

    if (have_flag(f_ptr->flags, FF_PERMANENT))
    {
        /* Titanium */
        if (have_flag(mimic_f_ptr->flags, FF_PERMANENT)) msg_print("This stone is impenetrable.");

        /* Map border (mimiccing Permanent wall) */
        else msg_print("You can't tunnel through that!");
    }

    /* Dig or tunnel */
    else if (have_flag(f_ptr->flags, FF_CAN_DIG))
    {
        /* Dig */
        if (p_ptr->skill_dig > randint0(20 * power))
        {
            msg_format("You clear away the %s.", name);
            cave_alter_feat(y, x, FF_TUNNEL);
            p_ptr->update |= PU_FLOW; // Update travel paths
        }
        else
        {
            msg_format("You dig into the %s.", name);
            more = TRUE;
        }
    }
    else
    {
        bool tree = have_flag(mimic_f_ptr->flags, FF_TREE);

        /* Tunnel */
        if (p_ptr->skill_dig > power + randint0(40 * power))
        {
            if (tree) msg_format("You have cut down the %s.", name);
            else
            {
                msg_print("You have finished the tunnel.");
                p_ptr->update |= PU_FLOW;
            }

            /* Sound */
            if (have_flag(f_ptr->flags, FF_GLASS)) sound(SOUND_GLASS);

            /* Remove the feature */
            cave_alter_feat(y, x, FF_TUNNEL);

            virtue_add(VIRTUE_DILIGENCE, 1);
            virtue_add(VIRTUE_NATURE, -1);
        }
        else // Keep trying
        {
            /* Don't continue if it's completely futile */
            if (p_ptr->skill_dig > power) more = TRUE;

            if (tree)
            {
                msg_format("You chop away at the %s.", name);
                if (randint0(100) < 25) search(); // Occasionally search
            }
            else
            {
                msg_format("You %stunnel into the %s.", more ? "" : "futilely attempt to ", name);
            }
        }
    }

    if (is_hidden_door(c_ptr)) if (randint0(100) < 25) search(); // Occasionally search

    p_inc_fatigue(MUT_EASY_TIRING2, 3);

    return more;
}


/*
 * Tunnels through "walls" (including rubble and closed doors)
 *
 * Note that you must tunnel in order to hit invisible monsters
 * in walls, though moving into walls still takes a turn anyway.
 *
 * Digging is very difficult without a "digger" weapon, but can be
 * accomplished by strong players using heavy weapons.
 */
void do_cmd_tunnel(void)
{
    int dir;
    bool more = FALSE;

    if (p_ptr->special_defense & KATA_MUSOU) set_action(ACTION_NONE);

    /* Allow repeated command */
    if (command_arg)
    {
        /* Set repeat count */
        command_rep = command_arg - 1;
        p_ptr->redraw |= PR_STATE;
        command_arg = 0; // Cancel the arg
    }

    /* Get a direction to tunnel, or Abort */
    if (get_rep_dir(&dir, FALSE))
    {
        int y = py + ddy[dir];
        int x = px + ddx[dir];
        cave_type *c_ptr = &cave[y][x];
      
        /* Feature code (applying "mimic" field) */
        s16b feat = get_feat_mimic(c_ptr);

        /* No tunnelling through doors */
        if (have_flag(f_info[feat].flags, FF_DOOR))         msg_print("You cannot tunnel through doors.");
        else if (!have_flag(f_info[feat].flags, FF_TUNNEL)) msg_print("You can't tunnel through that.");
        else if (c_ptr->m_idx)
        {
            msg_print("There is a monster in the way!");
            energy_use = 100;
            py_attack(y, x, 0);
        }
        else more = do_cmd_tunnel_aux(y, x); // Try digging
    }

    if (!more) disturb(0, 0); // Cancel repetition unless we can continue
}


#ifdef ALLOW_EASY_OPEN /* TNB */

/*
 * easy_open_door --
 *
 *    If there is a jammed/closed/locked door at the given location,
 *    then attempt to unlock/open it. Return TRUE if an attempt was
 *    made (successful or not), otherwise return FALSE.
 *
 *    The code here should be nearly identical to that in
 *    do_cmd_open_test() and do_cmd_open_aux().
 */
bool easy_open_door(int y, int x, int dir)
{
    cave_type *c_ptr = &cave[y][x];
    feature_type *f_ptr = &f_info[c_ptr->feat];

    /* Must be a closed door */
    if (!is_closed_door(c_ptr->feat)) return FALSE;

    if (p_ptr->prace == RACE_MON_VORTEX) return do_cmd_bash_aux(y, x, dir);

    /* Jammed door */
    if (!have_flag(f_ptr->flags, FF_OPEN))
    {
        cmsg_format(TERM_L_UMBER, "The %s appears to be stuck.", f_name + f_info[get_feat_mimic(c_ptr)].name);
    }

    /* Locked door */
    else if (f_ptr->power)
    {
        int skill = p_ptr->skills.dis;
        if (p_ptr->blind    || no_lite())    skill /= 10;
        if (p_ptr->confused || p_ptr->image) skill /= 10;

        int dc = f_ptr->power;
        dc = skill - (dc * 4);
        if (dc < 2) dc = 2; // Minimum 2% chance

        if (randint0(100) < dc)
        {
            cmsg_print(TERM_L_UMBER, "You have picked the lock.");
            cave_alter_feat(y, x, FF_OPEN);
            sound(SOUND_OPENDOOR);
            gain_exp(1);
        }
        else
        {
            if (flush_failure && !travel.run) flush();
            cmsg_print(TERM_L_UMBER, "You failed to pick the lock.");
        }
    }
    else // Closed door
    {
        cave_alter_feat(y, x, FF_OPEN); // Open the door
        sound(SOUND_OPENDOOR);
    }

    return TRUE; // Result
}

#endif /* ALLOW_EASY_OPEN -- TNB */


/*
 * Perform the basic "disarm" command
 *
 * Assume destination is a visible trap
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_disarm_chest(int y, int x, s16b o_idx)
{
    int i, j;

    bool more = FALSE;

    object_type *o_ptr = &o_list[o_idx];


    /* Take a turn */
    energy_use = 100;

    /* Get the "disarm" factor */
    i = p_ptr->skills.dis;

    /* Penalize some conditions */
    if (p_ptr->blind || no_lite()) i = i / 10;
    if (p_ptr->confused || p_ptr->image) i = i / 10;

    /* Extract the difficulty */
    j = i - o_ptr->pval;

    /* Always have a small chance of success */
    if (j < 2) j = 2;

    /* Must find the trap first. */
    if (!object_is_known(o_ptr))
    {
        msg_print("I don't see any traps.");

    }

    /* Already disarmed/unlocked */
    else if (o_ptr->pval <= 0)
    {
        msg_print("The chest is not trapped.");

    }

    /* No traps to find. */
    else if (!chest_traps[o_ptr->pval])
    {
        msg_print("The chest is not trapped.");

    }

    /* Success (get a lot of experience) */
    else if (randint0(100) < j)
    {
        msg_print("You have disarmed the chest.");

        gain_exp(o_ptr->pval);
        o_ptr->pval = (0 - o_ptr->pval);
    }

    /* Failure -- Keep trying */
    else if ((i > 5) && (randint1(i) > 5))
    {
        /* We may keep trying */
        more = TRUE;
        if (flush_failure) flush();
        msg_print("You failed to disarm the chest.");

    }

    /* Failure -- Set off the trap */
    else
    {
        msg_print("You set off a trap!");

        sound(SOUND_FAIL);
        chest_trap(y, x, o_idx);
    }

    water_mana_action(FALSE, 5);

    /* Result */
    return (more);
}


/*
 * Perform the basic "disarm" command
 *
 * Assume destination is a visible trap
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
#ifdef ALLOW_EASY_DISARM /* TNB */

bool do_cmd_disarm_aux(int y, int x, int dir)

#else /* ALLOW_EASY_DISARM -- TNB */

static bool do_cmd_disarm_aux(int y, int x, int dir)

#endif /* ALLOW_EASY_DISARM -- TNB */
{
    /* Get grid and contents */
    cave_type *c_ptr = &cave[y][x];

    /* Get feature */
    feature_type *f_ptr = &f_info[c_ptr->feat];

    /* Access trap name */
    cptr name = (f_name + f_ptr->name);

    /* Extract trap "power" */
    int power = f_ptr->power;

    bool more = FALSE;

    /* Get the "disarm" factor */
    int i = p_ptr->skills.dis;

    int j;

    /* Take a turn */
    energy_use = 100;

    /* Penalize some conditions */
    if (p_ptr->blind || no_lite()) i = i / 10;
    if (p_ptr->confused || p_ptr->image) i = i / 10;

    /* Extract the difficulty */
    j = i - power;

    /* Always have a small chance of success */
    if (j < 2) j = 2;

    /* Success */
    if (randint0(100) < j)
    {
        /* Message */
        msg_format("You have disarmed the %s.", name);

        /* Reward */
        gain_exp(power);

        /* Remove the trap */
        cave_alter_feat(y, x, FF_DISARM);

#ifdef ALLOW_EASY_DISARM /* TNB */

        /* Move the player onto the trap */
        move_player(dir, easy_disarm, FALSE);

#else /* ALLOW_EASY_DISARM -- TNB */

        /* move the player onto the trap grid */
        move_player(dir, FALSE, FALSE);

#endif /* ALLOW_EASY_DISARM -- TNB */
    }

    /* Failure -- Keep trying */
    else if ((i > 5) && (randint1(i) > 5))
    {
        /* Failure */
        if (flush_failure) flush();

        /* Message */
        msg_format("You failed to disarm the %s.", name);

        /* We may keep trying */
        more = TRUE;
    }

    /* Failure -- Set off the trap */
    else
    {
        /* Message */
        msg_format("You set off the %s!", name);

#ifdef ALLOW_EASY_DISARM /* TNB */

        /* Move the player onto the trap */
        move_player(dir, easy_disarm, FALSE);

#else /* ALLOW_EASY_DISARM -- TNB */

        /* Move the player onto the trap */
        move_player(dir, FALSE, FALSE);

#endif /* ALLOW_EASY_DISARM -- TNB */
    }

    water_mana_action(FALSE, 5);

    /* Result */
    return (more);
}


/*
 * Disarms a trap, or chest
 */
void do_cmd_disarm(void)
{
    int dir;
    bool more = FALSE;

    if (p_ptr->special_defense & KATA_MUSOU) set_action(ACTION_NONE);

#ifdef ALLOW_EASY_DISARM /* TNB */
    /* Option: Pick a direction */
    if (easy_disarm)
    {
        int y, x;
        int count = count_dt(&y, &x, is_trap, TRUE) + count_chests(&y, &x, _CHEST_TRAPPED);
        if (count == 1) command_dir = coords_to_dir(y, x);
    }
#endif /* ALLOW_EASY_DISARM -- TNB */

    /* Allow repeated command */
    if (command_arg)
    {
        command_rep = command_arg - 1; // Set repeat count
        p_ptr->redraw |= PR_STATE;
        command_arg = 0; // Cancel the arg
    }

    /* Get a direction (or abort) */
    if (get_rep_dir(&dir, TRUE))
    {
        int y = py + ddy[dir];
        int x = px + ddx[dir];
        cave_type *c_ptr = &cave[y][x];

        /* Feature code (applying "mimic" field) */
        s16b feat = get_feat_mimic(c_ptr);

        s16b o_idx = chest_check(y, x, _CHEST_TRAPPED);

        if (!is_trap(feat) && !o_idx) msg_print("You see nothing there to disarm.");
        else if (c_ptr->m_idx && p_ptr->riding != c_ptr->m_idx)
        {
            msg_print("There is a monster in the way!");
            energy_use = 100;
            py_attack(y, x, 0);
        }
        else if (o_idx) more = do_cmd_disarm_chest(y, x, o_idx);
        else            more = do_cmd_disarm_aux(y, x, dir); // Disarm trap
    }

    if (!more) disturb(0, 0); // Cancel repeat unless told not to
}


/*
 * Perform the basic "bash" command
 *
 * Assume destination is a closed/locked/jammed door
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_bash_aux(int y, int x, int dir)
{
    /* Get grid */
    cave_type    *c_ptr = &cave[y][x];

    /* Get feature */
    feature_type *f_ptr = &f_info[c_ptr->feat];

    /* Hack -- Bash power based on strength */
    /* (Ranges from 3 to 20 to 100 to 200) */
    int bash = adj_str_blow[p_ptr->stat_ind[A_STR]];

    /* Extract door power */
    int dc = f_ptr->power;

    bool        more = FALSE;

    cptr name = f_name + f_info[get_feat_mimic(c_ptr)].name;

    /* Take a turn */
    energy_use = 100;

    msg_format("You smash into the %s!", name);

    /* Compare bash power to door power XXX XXX XXX */
    dc = (bash - (dc * 10));

    if (p_ptr->pclass == CLASS_BERSERKER) dc *= 2;
    if (p_ptr->prace == RACE_MON_VORTEX)  dc = 100;

    if (dc < 1) dc = 1; // 1% chance at minimum

    /* Hack -- attempt to bash down the door */
    if (randint0(100) < dc)
    {
        msg_format("The %s crashes open!", name);
        sound(have_flag(f_ptr->flags, FF_GLASS) ? SOUND_GLASS : SOUND_OPENDOOR);

        /* Break down the door */
        if ((randint0(100) < 50) || (feat_state(c_ptr->feat, FF_OPEN) == c_ptr->feat) || have_flag(f_ptr->flags, FF_GLASS))
        {
            cave_alter_feat(y, x, FF_BASH);
        }
        else cave_alter_feat(y, x, FF_OPEN);

        move_player(dir, FALSE, FALSE); // Fall through the door
    }

    /* Saving throw against stun */
    else if (randint0(100) < adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
         p_ptr->lev)
    {
        msg_format("The %s holds firm.", name);
        more = TRUE; // Allow repeated bashing
    }

    /* High dexterity yields coolness */
    else
    {
        msg_print("You are off-balance.");
        (void)set_paralyzed(randint1(4), FALSE);
    }

    water_mana_action(FALSE, 20);

    return more;
}


/*
 * Bash open a door, success based on character strength
 *
 * For a closed door, pval is positive if locked; negative if stuck.
 *
 * For an open door, pval is positive for a broken door.
 *
 * A closed door can be opened - harder if locked. Any door might be
 * bashed open (and thereby broken). Bashing a door is (potentially)
 * faster! You move into the door way. To open a stuck door, it must
 * be bashed. A closed door can be jammed (see do_cmd_spike()).
 *
 * Creatures can also open or bash doors, see elsewhere.
 */
void do_cmd_bash(void)
{
    int dir;
    bool more = FALSE;

    if (p_ptr->special_defense & KATA_MUSOU) set_action(ACTION_NONE);

    /* Allow repeated command */
    if (command_arg)
    {
        command_rep = command_arg - 1; // Set repeat count
        p_ptr->redraw |= PR_STATE;
        command_arg = 0; // Cancel the arg
    }

    /* Get a "repeated" direction */
    if (get_rep_dir(&dir, FALSE))
    {
        int y = py + ddy[dir];
        int x = px + ddx[dir];
        cave_type *c_ptr = &cave[y][x];

        /* Feature code (applying "mimic" field) */
        s16b feat = get_feat_mimic(c_ptr);

        /* Nothing useful */
        if (!have_flag(f_info[feat].flags, FF_BASH)) msg_print("You see nothing there to bash.");
        else if (c_ptr->m_idx)
        {
            msg_print("There is a monster in the way!");
            energy_use = 100;
            py_attack(y, x, 0);
        }
        else more = do_cmd_bash_aux(y, x, dir);
        
    }

    if (!more) disturb(0, 0); // Unless valid action taken, cancel bash
}


/*
 * Manipulate an adjacent grid in some way
 *
 * Attack monsters, tunnel through walls, disarm traps, open doors.
 *
 * Consider confusion XXX XXX XXX
 *
 * This command must always take a turn, to prevent free detection
 * of invisible monsters.
 */
void do_cmd_alter(void)
{
    int dir;
    bool more = FALSE;

    if (p_ptr->special_defense & KATA_MUSOU) set_action(ACTION_NONE);

    /* Allow repeated command */
    if (command_arg)
    {
        command_rep = command_arg - 1; // Set repeat count
        p_ptr->redraw |= PR_STATE;
        command_arg = 0; // Cancel the arg
    }

    /* Get a direction */
    if (get_rep_dir(&dir,TRUE))
    {
        int y = py + ddy[dir];
        int x = px + ddx[dir];
        cave_type *c_ptr = &cave[y][x];

        /* Feature code (applying "mimic" field) */
        s16b feat = get_feat_mimic(c_ptr);
        feature_type *f_ptr = &f_info[feat];

        /* Take a turn */
        energy_use = 100;

        if (c_ptr->m_idx) py_attack(y, x, 0);
        else if (have_flag(f_ptr->flags, FF_OPEN))   more = do_cmd_open_aux(y, x);
        else if (have_flag(f_ptr->flags, FF_BASH))   more = do_cmd_bash_aux(y, x, dir);
        else if (have_flag(f_ptr->flags, FF_TUNNEL)) more = do_cmd_tunnel_aux(y, x);
        else if (have_flag(f_ptr->flags, FF_CLOSE))  more = do_cmd_close_aux(y, x);
        else if (have_flag(f_ptr->flags, FF_DISARM)) more = do_cmd_disarm_aux(y, x, dir);
        else msg_print("You attack the empty air.");
    }

    if (!more) disturb(0, 0); // Cancel repetition unless we can continue
}

/*
 * Jam a closed door with a spike
 *
 * This command may NOT be repeated
 */
void do_cmd_spike(void)
{
    int dir;

    if (p_ptr->special_defense & KATA_MUSOU) set_action(ACTION_NONE);

    /* Get a "repeated" direction */
    if (get_rep_dir(&dir,FALSE))
    {
        int y, x;
        cave_type *c_ptr;
        s16b feat;

        /* Get location */
        y = py + ddy[dir];
        x = px + ddx[dir];

        /* Get grid and contents */
        c_ptr = &cave[y][x];

        /* Feature code (applying "mimic" field) */
        feat = get_feat_mimic(c_ptr);

        /* Require closed door */
        if (!have_flag(f_info[feat].flags, FF_SPIKE))
        {
            /* Message */
            msg_print("You see nothing there to spike.");

        }
        /* Is a monster in the way? */
        else if (c_ptr->m_idx)
        {
            /* Take a turn */
            energy_use = 100;

            /* Message */
            msg_print("There is a monster in the way!");

            /* Attack */
            py_attack(y, x, 0);
        }

        /* Go for it */
        else
        {
            slot_t slot = pack_find_obj(TV_SPIKE, SV_ANY);
            obj_ptr spike;
            if (!slot)
            {
                msg_print("You have no spikes.");
                return;
            }

            /* Take a turn */
            energy_use = 100;

            /* Successful jamming */
            msg_format("You jam the %s with a spike.", f_name + f_info[feat].name);

            cave_alter_feat(y, x, FF_SPIKE);

            spike = pack_obj(slot);
            spike->number--;
            obj_release(spike, 0);
        }
    }
}



/*
 * Support code for the "Walk" and "Jump" commands
 */
static void do_cmd_walk_aux(int dir, bool pickup)
{
    /* Take a turn */
    energy_use = 100;

    if ((dir != 5) && (p_ptr->special_defense & KATA_MUSOU)) set_action(ACTION_NONE);

    /* Hack -- In small scale wilderness it takes MUCH more time to move */
    if (p_ptr->wild_mode) energy_use *= ((MAX_HGT + MAX_WID) / 2);
    if (!p_ptr->riding)
    {
        if (mut_present(MUT_LIMP)) energy_use += (energy_use / 9);

        if (p_ptr->action == ACTION_QUICK_WALK) energy_use = (p_ptr->pclass == CLASS_NINJA_LAWYER) ? 
             energy_use * (60-(p_ptr->lev/2)) / 100 : energy_use * (45-(p_ptr->lev/2)) / 100;
        if (p_ptr->action == ACTION_STALK) energy_use = energy_use * (150 - p_ptr->lev) / 100;
        if (weaponmaster_get_toggle() == TOGGLE_SHADOW_STANCE)
            energy_use = energy_use * (45-(p_ptr->lev/2)) / 100;

        if (p_ptr->quick_walk)
            energy_use = energy_use * 60 / 100;

        if (personality_is_(PERS_CRAVEN)) energy_use = energy_use * 21 / 25;

        if (prace_is_(RACE_MON_GOLEM))
            energy_use *= 2;
    }

    move_player(dir, pickup, FALSE);
}

void do_cmd_walk(bool pickup)
{
    int dir;

    bool more = FALSE;

    /* Hack - assume no unwanted auto-running */
    run_count = 5;

    /* Allow repeated command */
    if (command_arg)
    {
        /* Set repeat count */
        command_rep = command_arg - 1;

        /* Redraw the state */
        p_ptr->redraw |= (PR_STATE);

        /* Cancel the arg */
        command_arg = 0;
    }

    /* Get a "repeated" direction (hacked to allow targeting) */
	get_aim_dir(&dir);
    if (dir == 5)
	{
		int i;
		int dx, dy, sx, sy;
		dx = abs(px - target_col);
		dy = abs(py - target_row);
		sx = ((target_col == px) || (dx < dy)) ? 0 : ((target_col > px) ? 1 : -1);
		sy = ((target_row == py) || (dy < dx)) ? 0 : ((target_row > py) ? 1 : -1);
		for (i = 1; i <= 9; i++)
		{
			if ((sx == ddx[i]) && (sy == ddy[i])) dir = i;
		}
		do_cmd_walk_aux(dir, pickup);
	}
	else if (dir>0 && dir != 5)
	{
		do_cmd_walk_aux(dir, pickup);
		more = TRUE;
	}

    /* Hack again -- Is there a special encounter ??? */
    if (p_ptr->wild_mode && !cave_have_flag_bold(py, px, FF_TOWN))
    {
        int lvl = wilderness_level(px, py);
        int tmp = MAX(1, 120 + p_ptr->lev*10 - lvl + 5);

        if (wilderness[py][px].road)
            tmp *= 8;

        if (!is_daytime())
            tmp /= 2;

        if (( lvl + 5 > p_ptr->lev / 2
          && randint0(tmp) < 21 - p_ptr->skills.stl ) ||
            ((thrall_mode) && (wilderness[py][px].terrain == TERRAIN_DEEP_WATER)
             && (one_in_(MAX(3, p_ptr->pspeed - 110)))))
        {
            /* Inform the player of his horrible fate :=) */
            msg_print("You are ambushed!");

            /* Go into large wilderness view */
            p_ptr->oldpy = rand_range(15, MAX_HGT - 15);
            p_ptr->oldpx = rand_range(15, MAX_WID - 15);
            change_wild_mode();

            /* Give first move to monsters */
            energy_use = 100;

            /* Hack -- set the encounter flag for the wilderness generation */
            generate_encounter = TRUE;
        }
    }

    /* Cancel repeat unless we may continue */
    if (!more) disturb(0, 0);
}

/*
 * Start running. For confused or randomly moving
 * players, we disallow running in a random direction.
 * They will simply stumble a single step instead.
 */
void do_cmd_run(void)
{
    int dir;

    if (!online_macros && (++run_count == 4) && !rogue_like_commands)
    {
        msg_print("The game has detected multiple calls to the 'Run' command");
        msg_print("without any calls to the 'Walk' command, a possible sign");
        msg_print("of undesired autorunning. If you are playing on the angband.live");
        msg_print("online server, you can turn on the online_macros option to");
        msg_print("disable autorunning. (If your keyboard has a Num Lock key,");
        msg_print("you should toggle it instead of online_macros.)\n\n");
        msg_print("This message will not appear again, but you can toggle online_macros");
        msg_print("at any time in the Input Options menu.");
        msg_print(NULL);
        if (msg_prompt("Turn on the online_macros option? <color:y>[y/n]</color>", "ny", PROMPT_DEFAULT) == 'y')
            online_macros = TRUE;
        run_count = 5;
        p_ptr->redraw |= PR_MAP;
        handle_stuff();
    }

    if (p_ptr->special_defense & KATA_MUSOU) set_action(ACTION_NONE);

    switch (get_rep_dir(&dir,FALSE))
    {
    case GET_DIR_OK:
        running = (command_arg ? command_arg : 1000);
        run_step(dir);
        break;
    case GET_DIR_RANDOM:
        do_cmd_walk_aux(dir, FALSE);
        break;
    }
}


/*
 * Stay still. Search. Enter stores.
 * Pick up treasure if "pickup" is true.
 */
void do_cmd_stay(bool pickup)
{
    u32b mpe_mode = MPE_STAYING | MPE_ENERGY_USE;

    /* Allow repeated command */
    if (command_arg)
    {
        /* Set repeat count */
        command_rep = command_arg - 1;

        /* Redraw the state */
        p_ptr->redraw |= PR_STATE;

        /* Cancel the arg */
        command_arg = 0;
    }

    /* Take a turn */
    energy_use = 100;

    if (pickup) mpe_mode |= MPE_DO_PICKUP;
    (void)move_player_effect(py, px, mpe_mode);
}

/* Get Object(s).
 * Historically, 'g' was 'stay still (flip pickup)' which makes little sense */
static bool _travel_next_obj(int mode)
{
    int best_idx = -1, best_dist = 0;
    for (int i = 0; i < max_o_idx; i++)
    {
        object_type *o_ptr = &o_list[i];
        int          dist = 0;

        if (!o_ptr->k_idx) continue;
        if (!(o_ptr->marked & OM_FOUND)) continue;
        if (mode == TRAVEL_MODE_AMMO)
        {
            if (!o_ptr->inscription) continue;
            if (!strstr(quark_str(o_ptr->inscription), "=g")) continue;
            if (o_ptr->loc.x == px && o_ptr->loc.y == py)
            {
                /* Full pack aborts the travel sequence */
                return FALSE;
            }
        }
        else if (mode == TRAVEL_MODE_AUTOPICK && o_ptr->tval != TV_GOLD)
        {
            int j = is_autopick(o_ptr);
            int _check_mode = DO_AUTOPICK;

            if (j < 0) continue;
            /* Worthless items marked for autodestruction cause loops if
             * they are not actually destroyed... */
            if ((!always_pickup) || (destroy_items)) _check_mode |= DO_AUTODESTROY;
            if (!(autopick_list[j].action & (_check_mode))) continue;
            if (o_ptr->loc.x == px && o_ptr->loc.y == py)
            {
                /* Full pack aborts the travel sequence */
                if (autopick_list[j].action & DO_AUTOPICK)
                    return FALSE;
                continue; /* paranoia ... we should have destroyed this object */
            }

            /* Avoid loop when we try to destroy indestructible items */
            if ((!(autopick_list[j].action & DO_AUTOPICK)) && (object_is_artifact(o_ptr)) && ((obj_is_identified(o_ptr)) || (o_ptr->ident & IDENT_SENSE)))
                continue;
        }

        dist = distance(py, px, o_ptr->loc.y, o_ptr->loc.x);
        if (!projectable(py, px, o_ptr->loc.y, o_ptr->loc.x))
        {
            if (mode == TRAVEL_MODE_AUTOPICK) continue;
            else if (dist > 18) continue;
        }

        if (best_idx == -1 || dist < best_dist)
        {
            best_idx = i;
            best_dist = dist;
        }
    }
    if (best_idx == -1) return FALSE;
    travel_begin(mode, o_list[best_idx].loc.x, o_list[best_idx].loc.y);
    return TRUE;
}

bool _has_unmarked_floor(int y, int x)
{
    int count = 0;
    for (int dy = -1; dy <= 1; dy++)
        for (int dx = -1; dx <= 1; dx++)
            if (!(cave[y+dy][x+dx].info & CAVE_MARK)) count++;
    return (count > 0 && count < 8);
}

int _auto_explore_affinity(int y, int x) {
  cave_type *c_ptr = &cave[y][x];
  
  // Autoexplore will not travel to grids with affinity less than 1
  if(is_known_trap(c_ptr))                 return 0;
  if(is_hidden_door(c_ptr))                return 0;
  
  // May as well check out this item while we're here
  if(cave_have_flag_grid(c_ptr, FF_HAS_GOLD)) return 400;
  if(cave_have_flag_grid(c_ptr, FF_HAS_ITEM)) return 500;
  
  // Autoexplore prefers to open doors
  if(is_jammed_door(c_ptr->feat))          return 150; //
  if(is_closed_door(c_ptr->feat))          return 200; //
  if(cave_have_flag_grid(c_ptr, FF_OPEN))  return 120; // open door
  
  if(!p_ptr->levitation) {
    if(cave_have_flag_grid(c_ptr, FF_DEEP))  return 20;
    
    if (cave_have_flag_grid(c_ptr, FF_SHALLOW) ||
        cave_have_flag_grid(c_ptr, FF_SLUSH)   ||
        cave_have_flag_grid(c_ptr, FF_SNOW)) return 40;
  }
  
  if(cave_have_flag_grid(c_ptr, FF_WEB))   return 10;
  if(cave_have_flag_grid(c_ptr, FF_TREE))  return 30;
  if(cave_have_flag_grid(c_ptr, FF_PLANT)) return 90;
  
  return 100;
}

bool _travel_continue(bool initial)
{
  update_stuff();
  
  int ty = -1, tx = -1;
  int r, best_affinity = -1;
  int d0 = chome[find_prevdir];
  int debug_newdir = -1, debug_dy = 0, debug_dx = 0;
  
  // -1 : 0,1,2 => 0,1,2 => -1,0,1 : right, center, left
  // +1 : 2,3,4 => 2,0,1 => 1,-1,0 : left, right, center
  
  // First try continuing in the same direction
#if 0
  for(int i = 1; i <= 3; i++) {
    int new_dir = cycle[d0 + ((i+1)%3)-1]; // 1,2,3 => 1,2,0 => 0,1,-1 : center, left, right
#else
  int dd[] = { 0, 1, -1 }; // center, left, right
  for(int i = 0; i < 3; i++) {
    int new_dir = cycle[d0 + dd[i]];
#endif
    int y = py + ddy[new_dir];
    int x = px + ddx[new_dir];
    if (in_bounds(y, x) && can_travel(y, x) && _has_unmarked_floor(y, x)) {
      int affinity = _auto_explore_affinity(y, x);
      if(affinity > best_affinity) {
        best_affinity = affinity;
        ty = y;
        tx = x;
        debug_newdir = new_dir;
        debug_dy = ddy[new_dir];
        debug_dx = ddx[new_dir];
      }
    }
  }
  //if (best_affinity > 0) msg_format("prevdir: %d, newdir: %d, dy: %+d, dx: %+d\n", find_prevdir, debug_newdir, debug_dy, debug_dx);
  if (best_affinity > 0) goto done;
  
  // Then check nearby tiles
  // Autoexplore has an affinity for certain grids like doors
  for(r = 1; r <= 3; r++) {
    for (int dy = -r; dy <= r; dy+=2*r) {
      for (int dx = -r; dx <= r; dx++) {
        int y = py+dy, x = px+dx;
        if (in_bounds(y, x) && can_travel(y, x) && _has_unmarked_floor(y, x)) {
          int affinity = _auto_explore_affinity(y, x);
          if(affinity > best_affinity) {
            best_affinity = affinity;
            ty = y;
            tx = x;
          }
        }
      }
    }
    for (int dx = -r; dx <= r; dx+=2*r) {
      for (int dy = -r+1; dy <= r-1; dy++) {
        int y = py+dy, x = px+dx;
        if (in_bounds(y, x) && can_travel(y, x) && _has_unmarked_floor(y, x)) {
          int affinity = _auto_explore_affinity(y, x);
          if(affinity > best_affinity) {
            best_affinity = affinity;
            ty = y;
            tx = x;
          }
        }
      }
    }
  }
  if (best_affinity > 0) goto done;
  
  // Then check a larger radius around the player
  for(; r < cur_hgt; r++) {
    for (int dy = -r; dy <= r; dy+=2*r) {
      int y = py+dy;
      if (!in_bounds(y, px)) continue;
      for (int x = MAX(1, px-r); x <= MIN(cur_wid-2, px+r); x++) {
        if (can_travel(y, x) && _has_unmarked_floor(y, x)) {
          ty = y;
          tx = x;
          goto done;
        }
      }
    }
    for (int dx = -r; dx <= r; dx+=2*r) {
      int x = px+dx;
      if (!in_bounds(py, x)) continue;
      for (int y = MAX(1, py-r); y <= MIN(cur_hgt-2, py+r); y++) {
        if (can_travel(y, x) && _has_unmarked_floor(y, x)) {
          ty = y;
          tx = x;
          goto done;
        }
      }
    }
  }
  
  // Nothing nearby, search the entire level
  int min_dist = cur_hgt + cur_wid;
  for (int y = 1; y < cur_hgt-1; y++) {
    for (int x = 1; x < cur_wid-1; x++) {
      if (can_travel(y, x) && _has_unmarked_floor(y, x)) {
        int dist = distance(py, px, y, x);
        if (dist < min_dist) {
          min_dist = dist;
          ty = y;
          tx = x;
        }
      }
    }
  }
  if (ty < 0 || tx < 0) return FALSE;
  
done:;
  travel_begin(TRAVEL_MODE_AUTOEXPLORE, tx, ty);
  if(initial) travel.run = 255;
  return TRUE;
}


void do_cmd_auto_explore(void)
{
    if (!_travel_continue(TRUE)) msg_print("Exploration complete!");
}

void do_cmd_get(void)
{
    if (!cave[py][px].o_idx)
        msg_print("You see no objects here. Try <color:keypress>^G</color> to auto-get nearby objects.");
    (void)pack_get_floor();
}
void do_cmd_autoget(void)
{
    /* Get any objects under foot first ... this is the old
     * 'g' behavior sans interaction with features (e.g. re-
     * enter a shop) */
    if (cave[py][px].o_idx || p_ptr->wizard)
    {
        if (!pack_get_floor()) return;
    }
    /* Now, auto pickup nearby objects by iterating
     * the travel command */
    if (auto_get_objects)
        _travel_next_obj(TRAVEL_MODE_AUTOPICK);
    else if (auto_get_ammo)
        _travel_next_obj(TRAVEL_MODE_AMMO);
    else
    {
        msg_print("<color:B>Warning:</color> You have specified neither the "
            "<color:keyword>auto_get_ammo</color> nor the <color:keyword>"
            "auto_get_objects</color> options. With neither option set, "
            "<color:keypress>^G</color> behaves just like the normal "
            "<color:keypress>g</color>et command.");
    }
}

/*
 * Resting allows a player to safely restore his hp    -RAK-
 */
void do_cmd_rest(void)
{
    int tmp;
    if (REPEAT_PULL(&tmp)) command_arg = tmp;

    set_action(ACTION_NONE);
    if (weaponmaster_get_toggle() == TOGGLE_SHADOW_STANCE) weaponmaster_set_toggle(TOGGLE_NONE);

    if (p_ptr->pclass == CLASS_BARD && (p_ptr->magic_num1[0] || p_ptr->magic_num1[1]))
    {
        bard_stop_singing();
    }

    /* Hex */
    if (hex_spelling_any()) stop_hex_spell_all();

    warlock_stop_singing();

    /* Prompt for time if needed */
    if (command_arg == 0)
    {
        cptr p = "<color:y>Rest</color> (0-9999, '*' for HP/SP, '&' as needed): ";

        char out_val[5];
        strcpy(out_val, "&");

        if (!msg_input_numpad(p, out_val, 5)) return;

        /* Rest until done */
        if (out_val[0] == '&') command_arg = -2;

        /* Rest a lot */
        else if (out_val[0] == '*') command_arg = -1;

        /* Rest some */
        else
        {
            command_arg = atoi(out_val);
            if (command_arg <= 0) return;
        }
        REPEAT_PUSH(command_arg);
    }

    if (command_arg > 9999) command_arg = 9999;

    /* Mimickry blocks all regeneration (hp and sp)!
       We can either block this command since resting will stop
       immediately (cf process_player() in dungeon.c) or, more
       conveniently, we can stop mimicry, though this will tend
       to expose the player!
     */
    if (mimic_no_regen() && command_arg < 0)
    {
        bool clear = TRUE;
        if (command_arg == -2)
        {
            if (p_ptr->blind ||
                p_ptr->confused ||
                p_ptr->poisoned ||
                p_ptr->afraid ||
                p_ptr->stun ||
                p_ptr->cut ||
                player_slow() ||
                p_ptr->paralyzed ||
                p_ptr->image ||
                p_ptr->word_recall ||
                p_ptr->alter_reality)
            {
                clear = FALSE;
            }
        }

        if (clear) mimic_race(MIMIC_NONE, "You cannot rest while maintaining your current form.");
    }

    if (p_ptr->special_defense & NINJA_S_STEALTH) set_superstealth(FALSE);

    if (p_ptr->filibuster) set_filibuster(FALSE);

    /* Take a turn XXX XXX XXX (?) */
    energy_use = 100;

    /* The sin of sloth */
    if (command_arg > 100) virtue_add(VIRTUE_DILIGENCE, -1);

    /* Why are you sleeping when there's no need?  WAKE UP!*/
    if ((p_ptr->chp == p_ptr->mhp) &&
        (p_ptr->csp == p_ptr->msp) &&
        !p_ptr->blind && !p_ptr->confused &&
        !p_ptr->poisoned && !p_ptr->afraid &&
        !p_ptr->stun && !p_ptr->cut &&
        !player_slow() && !p_ptr->paralyzed &&
        !p_ptr->image && !p_ptr->word_recall &&
        !p_ptr->alter_reality &&
        !magic_eater_can_regen())
    {
        virtue_add(VIRTUE_DILIGENCE, -1);
    }
    /* Save the rest code */
    resting = command_arg;
    p_ptr->action = ACTION_REST;

    /* Recalculate bonuses */
    p_ptr->update |= (PU_BONUS);

    /* Redraw the state */
    p_ptr->redraw |= (PR_STATE);

    /* Handle stuff */
    handle_stuff();

    /* Refresh */
    Term_fresh();
}


/*
 * Determines the odds of an object breaking when thrown at a monster
 *
 * Note that artifacts never break, see the "drop_near()" function.
 */
int breakage_chance(object_type *o_ptr)
{
    if (obj_is_art(o_ptr)) return 0;
    if (shoot_hack == SP_KILL_WALL) return 100;
    if (shoot_hack == SP_EXPLODE) return 100;
    if (shoot_hack == SP_PIERCE) return 100;
    if (shoot_hack == SP_FINAL) return 100;
    if (shoot_hack == SP_NEEDLE) return 100;

    if (shoot_hack == SHOOT_SHATTER) return 100;
    if (weaponmaster_get_toggle() == TOGGLE_EXPLODING_BOLT) return 100;
    if (shoot_hack == SHOOT_ELEMENTAL) return 100;
    if (weaponmaster_get_toggle() == TOGGLE_OVERDRAW) return 100;

    if (o_ptr->name2 == EGO_AMMO_ENDURANCE) return 0;
    if (shoot_hack == SP_EVILNESS) return 40;
    if (shoot_hack == SP_HOLYNESS) return 40;
    if (o_ptr->name2 == EGO_AMMO_EXPLODING) return 100;

    /* Examine the item type */
    switch (o_ptr->tval)
    {
        /* Always break */
        case TV_FLASK:
        case TV_POTION:
        case TV_BOTTLE:
        case TV_FOOD:
        case TV_JUNK:
            return 100;

        /* Often break */
        case TV_LITE:
        case TV_SCROLL:
        case TV_SKELETON:
            return 50;

        /* Sometimes break */
        case TV_WAND:
        case TV_SPIKE:
            return 25;
        case TV_ARROW:
            return 20 * MAX(0, p_ptr->shooter_info.breakage) / 100;

        /* Rarely break */
        case TV_SHOT:
        case TV_BOLT:
            return 10 * MAX(0, p_ptr->shooter_info.breakage) / 100;
        default:
            return 10;
    }
}


static s16b tot_dam_aux_shot(object_type *o_ptr, int tdam, monster_type *m_ptr)
{
    int mult = 100;
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    u32b flgs[OF_ARRAY_SIZE];

    /* Extract the flags */
    missile_flags(o_ptr, flgs);

    /* Some "weapons" and "ammo" do extra damage */
    switch (o_ptr->tval)
    {
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        {
            slay_type _slay = slay_list[0];
            int i;

            for (i = 0;; i++)
            {
                int my_mult = 100;
                _slay = slay_list[i];
                if (!_slay.tier) break; /* the only exit from this loop! */
                if (!_slay.tester(r_ptr, m_ptr, FALSE)) continue;
                if ((_slay.kill_flag > 0) && (have_flag(flgs, _slay.kill_flag)))
                {
                    char oppi[80];
                    my_mult = slay_tiers[_slay.tier - 1].archery_kill;
                    if ( o_ptr->name1 == ART_BARD_ARROW
                      && m_ptr->r_idx == MON_SMAUG
                      && equip_find_art(ART_BARD) )
                    {
                        my_mult *= 5;
                    }
                    strcpy(oppi, format("slays <color:%c>*%^s*</color>", _slay.attr, _slay.kill_desc));
                    obj_learn_slay(o_ptr, _slay.kill_flag, oppi);
                }
                else if (have_flag(flgs, _slay.slay_flag))
                {
                    char oppi[80];
                    my_mult = slay_tiers[_slay.tier - 1].archery_slay;
                    if ((_slay.slay_flag == OF_BRAND_FIRE) && (r_ptr->flags3 & RF3_HURT_FIRE))
                    {
                        my_mult = slay_tiers[_slay.tier - 1].archery_kill;
                        mon_lore_3(m_ptr, RF3_HURT_FIRE);
                    }
                    else if ((_slay.slay_flag == OF_BRAND_COLD) && (r_ptr->flags3 & RF3_HURT_COLD))
                    {
                        my_mult = slay_tiers[_slay.tier - 1].archery_kill;
                        mon_lore_3(m_ptr, RF3_HURT_COLD);
                    }
                    else my_mult = slay_tiers[_slay.tier - 1].archery_slay;
                    if (!_slay.is_slay) obj_learn_slay(o_ptr, _slay.slay_flag, _slay.brand_learn);
                    else
                    {
                        strcpy(oppi, format("slays <color:%c>%^s</color>", _slay.attr, _slay.kill_desc));
                        obj_learn_slay(o_ptr, _slay.slay_flag, oppi);
                    }
                }
                if (my_mult > mult) mult = my_mult;
                if (my_mult > 100) _slay.tester(r_ptr, m_ptr, TRUE);
            }

            if (have_flag(flgs, OF_BRAND_MANA) || p_ptr->tim_force)
            {
                int cost = 1 + o_ptr->dd * o_ptr->ds / 2;
                if (cost <= p_ptr->csp)
                {
                    sp_player(-cost);
                    mult += 5;
                    obj_learn_slay(o_ptr, OF_BRAND_MANA, "is <color:B>Mana Branded</color>");
                }
            }

            break;
        }
    }

    /* Sniper */
    if (p_ptr->pclass == CLASS_SNIPER && shoot_hack)
    {
        int n = sniper_multiplier(shoot_hack, o_ptr, m_ptr) * 10; /* XXX */
        if (n > mult)
            mult = n;
    }

    /* Return the total damage */
    return (tdam * mult / 100);
}


/*
 * Fire an object from the pack or floor.
 *
 * You may only fire items that "match" your missile launcher.
 *
 * You must use slings + pebbles/shots, bows + arrows, xbows + bolts.
 *
 * See "calc_bonuses()" for more calculations and such.
 *
 * Note that "firing" a missile is MUCH better than "throwing" it.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Objects are more likely to break if they "attempt" to hit a monster.
 *
 * Rangers (with Bows) and Anyone (with "Extra Shots") get extra shots.
 *
 * The "extra shot" code works by decreasing the amount of energy
 * required to make each shot, spreading the shots out over time.
 *
 * Note that when firing missiles, the launcher multiplier is applied
 * after all the bonuses are added in, making multipliers very useful.
 *
 * Note that Bows of "Extra Might" get extra range and an extra bonus
 * for the damage multiplier.
 *
 * Note that Bows of "Extra Shots" give an extra shot.
 */
bool do_cmd_fire_aux1(obj_ptr bow, obj_ptr arrows)
{
    int dir;
    int tdis, tx, ty;

    tdis = bow_range(bow);
    project_length = tdis + 1;

    if (shoot_hack == SHOOT_DISINTEGRATE)
    {
        if (target_set(TARGET_DISI))
        {
            if (target_who > 0)
            {
                tx = m_list[target_who].fx;
                ty = m_list[target_who].fy;
            }
            else
            {
                tx = target_col;
                ty = target_row;
            }
        }
        else
        {
            energy_use = 0;
            return FALSE;
        }
    }
    else
    {
        if (!get_fire_dir(&dir))
        {
            energy_use = 0;
            if (shoot_hack == SP_AWAY) shoot_hack = SP_NONE;
            return FALSE;
        }

        /* Predict the "target" location */
        tx = px + 99 * ddx[dir];
        ty = py + 99 * ddy[dir];

        /* Check for "target request" */
        if ((dir == 5) && target_okay())
        {
            tx = target_col;
            ty = target_row;
        }
    }

    /* Don't shoot at my feet */
    if (tx == px && ty == py)
    {
        energy_use = 0;
        /* project_length is already reset to 0 */
        return FALSE;
    }

    if (!fear_allow_shoot())
    {
        msg_print("You are too scared!");
        energy_use = bow_energy(bow->sval)/NUM_SHOTS;
        if (shoot_hack == SP_AWAY) shoot_hack = SP_NONE;
        return FALSE;
    }

    do_cmd_fire_aux2(bow, arrows, px, py, tx, ty);
    obj_release(arrows, OBJ_RELEASE_QUIET);
    water_mana_action(FALSE, MIN(5, energy_use / 5));
    return TRUE;
}
void do_cmd_fire_aux2(obj_ptr bow, obj_ptr arrows, int sx, int sy, int tx, int ty)
{
    int  i, break_chance, y, x, ny, nx, prev_y, prev_x, dd, ds;
    int  tdis, tmul;
    int  bonus, chance;
    int  cur_dis, visible;
    bool no_energy = FALSE;
    int  num_shots = 1;
    bool hit_body = FALSE;
    bool return_ammo = FALSE;
    char o_name[MAX_NLEN];
    u16b path_g[512];
    int  flgs = PROJECT_PATH | PROJECT_THRU;
    int  msec = delay_time();
    bool stick_to = FALSE;

    /* Sniper - Cannot shoot a single arrow twice */
    if ((shoot_hack == SP_DOUBLE) && (arrows->number < 2)) shoot_hack = SP_NONE;
    if (shoot_hack == SP_DOUBLE) num_shots = 2;

    /* Describe the object */
    object_desc(o_name, arrows, OD_OMIT_PREFIX | OD_NO_PLURAL | OD_OMIT_INSCRIPTION);

    /* Base damage from thrown object plus launcher bonus */
    dd = arrows->dd;
    ds = arrows->ds;
    if (p_ptr->big_shot)
        ds += 2;

    /* Actually "fire" the object */
    bonus = (p_ptr->shooter_info.to_h + arrows->to_h + bow->to_h);

    switch (shoot_hack)
    {
    case SHOOT_BOUNCE:
        bonus -= 20 * shoot_count;
        break;
    case SHOOT_RUN:
        bonus -= 10;
        no_energy = TRUE;
        break;
    case SHOOT_MANY:
        bonus -= 10;
        no_energy = TRUE;
        break;
    case SHOOT_ALL:
        bonus -= 20;
        no_energy = TRUE;
        break;
    case SHOOT_RETALIATE:
        no_energy = TRUE;
        break;
    case SHOOT_VOLLEY:
    case SHOOT_TRANQUILIZE:
    case SHOOT_NEEDLE:
    case SHOOT_DISINTEGRATE:
    case SHOOT_SHATTER:
    case SHOOT_KNOCKBACK:
    case SHOOT_ELEMENTAL:
        no_energy = TRUE;
        energy_use = 100;
        break;
    case SHOOT_RAMA:
        bonus += 20;
        break;
    }

    if (weaponmaster_get_toggle() == TOGGLE_PIERCING_ARROW || shoot_hack == SHOOT_PIERCE)
        shoot_count = 0;

    if (weaponmaster_get_toggle() == TOGGLE_RAPID_SHOT)
    {
        int  frac;
        s16b energy_fire = bow_energy(bow->sval);

        bonus -= 20;
        /* In this mode, whenever the player fires, all of their shots go
           at a single target in rapid succession. Full energy is consumed, and
           the player gets a slight bonus to the number of shots. Think of
           a rapid fire machine gun :) */
        no_energy = TRUE;
        energy_use = 100;

        /* Calculate shots per round
           CTK: energy_fire has four decimal places implied
                p_ptr->num_fire only has two decimal places implied */
        num_shots = NUM_SHOTS * 100;  /* rescale to 4 decimal places */
        num_shots = num_shots * 120 / 100;  /* rapid fire gives 1.2x the number of shots */
        frac = (num_shots * 100 / energy_fire) % 100;
        num_shots /= energy_fire;

        if (randint1(100) < frac)
            num_shots++;
    }

    chance = p_ptr->skills.thb + bonus * BTH_PLUS_ADJ;
    chance += skills_bow_calc_bonus(bow->sval) * BTH_PLUS_ADJ;

    if (p_ptr->stun)
        chance -= chance * MIN(100, p_ptr->stun) / 150;

    /* Calculate the Multiplier */
    tmul = bow_mult(bow);

    /* Base range */
    tdis = bow_range(bow);
    project_length = tdis + 1;

    /* Get projection path length */
    if (shoot_hack == SHOOT_DISINTEGRATE)
        flgs |= PROJECT_DISI;

    tdis = project_path(path_g, project_length, py, px, ty, tx, flgs) - 1;

    project_length = 0; /* reset to default */

    /* Take a (partial) turn */
    if (!no_energy)
        energy_use = bow_energy(bow->sval) / NUM_SHOTS;

    /* Sniper - Difficult to shot twice at 1 turn */
    if (shoot_hack == SP_DOUBLE)  p_ptr->concent = (p_ptr->concent + 1) / 2;

    /* Sniper - Repeat shooting when double shots */
    for (i = 0; i < num_shots; i++)
    {
        obj_t arrow;

        /* Make sure there is ammo left over for this shot */
        if (!arrows->number)
        {
            msg_print("Your ammo has run out. Time to reload!");
            break;
        }

        /* Start at the source */
        y = sy;
        x = sx;

        /* Weaponmaster power: Ammo is not consumed */
        if ( (p_ptr->return_ammo || arrows->name2 == EGO_AMMO_RETURNING || arrows->name1 == ART_BRAHMA)
          && randint1(100) <= 50 + p_ptr->lev/2 )
        {
            return_ammo = TRUE;
        }

        arrow = *arrows;
        arrow.number = 1;
        if (!return_ammo)
            arrows->number--;

        stats_on_use(arrows, 1);

        /* Sound */
        sound(SOUND_SHOOT);

        /* Hack -- Handle stuff */
        handle_stuff();

        /* Save the old location */
        prev_y = y;
        prev_x = x;

        /* The shot does not hit yet */
        hit_body = FALSE;

        /* Travel until stopped */
        for (cur_dis = 0; cur_dis <= tdis; )
        {
            cave_type *c_ptr;

            if (weaponmaster_get_toggle() != TOGGLE_PIERCING_ARROW && shoot_hack != SHOOT_PIERCE)
            {
                /* Hack -- Stop at the target */
                if ((y == ty) && (x == tx)) break;
            }

            /* Calculate the new location (see "project()") */
            ny = y;
            nx = x;
            /* Why, after having calculated a projection path, do we not use it?????
            mmove2(&ny, &nx, sy, sx, ty, tx);*/
            ny = GRID_Y(path_g[cur_dis]);
            nx = GRID_X(path_g[cur_dis]);

            /* Shatter Arrow */
            if (shoot_hack == SP_KILL_WALL)
            {
                c_ptr = &cave[ny][nx];
                if (cave_have_flag_grid(c_ptr, FF_HURT_ROCK) && !c_ptr->m_idx)
                {
                    if (c_ptr->info & (CAVE_MARK)) msg_print("Wall rocks were shattered.");
                    c_ptr->info &= ~(CAVE_MARK);
                    p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE);
                    cave_alter_feat(ny, nx, FF_HURT_ROCK);
                    hit_body = TRUE;
                    break;
                }
            }

            if (shoot_hack == SHOOT_DISINTEGRATE)
            {
                c_ptr = &cave[ny][nx];
                if (cave_have_flag_grid(c_ptr, FF_HURT_ROCK) && !c_ptr->m_idx)
                {
                    c_ptr->info &= ~(CAVE_MARK);
                    p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE);
                    cave_alter_feat(ny, nx, FF_HURT_ROCK);
                }
            }

            /* Stopped by walls/doors */
            if (!cave_have_flag_bold(ny, nx, FF_PROJECT) && !cave[ny][nx].m_idx) break;

            /* Advance the distance */
            cur_dis++;

            /* Sniper */
            if (shoot_hack == SP_LITE)
            {
                cave[ny][nx].info |= (CAVE_GLOW);
                note_spot(ny, nx);
                lite_spot(ny, nx);
            }

            /* The player can see the (on screen) missile */
            if (panel_contains(ny, nx) && player_can_see_bold(ny, nx))
            {
                char c = object_char(&arrow);
                byte a = object_attr(&arrow);

                /* Draw, Hilite, Fresh, Pause, Erase */
                print_rel(c, a, ny, nx);
                move_cursor_relative(ny, nx);
                Term_fresh();
                Term_xtra(TERM_XTRA_DELAY, msec);
                lite_spot(ny, nx);
                Term_fresh();
            }
            /* The player cannot see the missile */
            else
            {
                /* Pause anyway, for consistancy */
                Term_xtra(TERM_XTRA_DELAY, msec);
            }

            /* Sniper */
            if (shoot_hack == SP_KILL_TRAP)
            {
                project(0, 0, ny, nx, 0, GF_KILL_TRAP,
                    (PROJECT_JUMP | PROJECT_HIDE | PROJECT_GRID | PROJECT_ITEM));
            }
            if (shoot_hack == SP_EVILNESS)
            {
                cave[ny][nx].info &= ~(CAVE_GLOW | CAVE_MARK);
                note_spot(ny, nx);
                lite_spot(ny, nx);
            }

            /* Save the old location */
            prev_y = y;
            prev_x = x;

            /* Save the new location */
            x = nx;
            y = ny;

            /* Hack: Shoot all monsters fires over the heads of intervening monsters */
            if (shoot_hack == SHOOT_ALL && (x != tx || y != ty)) continue;
            if (shoot_hack == SHOOT_VOLLEY && (x != tx || y != ty)) continue;

            /* Monster here, Try to hit it */
            if (cave[y][x].m_idx)
            {
                int armour;
                bool hit = FALSE;
                cave_type *c_ptr = &cave[y][x];

                monster_type *m_ptr = &m_list[c_ptr->m_idx];
                monster_race *r_ptr = &r_info[m_ptr->r_idx];

                /* Check the visibility */
                visible = m_ptr->ml;

                /* Note the collision */
                hit_body = TRUE;

                if (MON_CSLEEP(m_ptr))
                {
                    if (!(r_ptr->flags3 & RF3_EVIL) || one_in_(5)) virtue_add(VIRTUE_COMPASSION, -1);
                    if (!(r_ptr->flags3 & RF3_EVIL) || one_in_(5)) virtue_add(VIRTUE_HONOUR, -1);
                }

                skills_bow_gain(bow->sval, r_ptr->level);
                if (p_ptr->riding)
                    skills_riding_gain_archery(r_ptr);

                armour = mon_ac(m_ptr);
                if (p_ptr->concent)
                {
                    armour *= (10 - p_ptr->concent);
                    armour /= 10;
                }

                if ( p_ptr->painted_target
                  && p_ptr->painted_target_idx == c_ptr->m_idx
                  && p_ptr->painted_target_ct >= 3)
                {
                    if (randint1(100) <= 95) hit = TRUE;
                }
                else if (weaponmaster_is_(WEAPONMASTER_BOWS) && cur_dis == 1)
                {
                    hit = TRUE;
                }
                else
                {
                    int chance2 = chance;

                    if (weaponmaster_is_(WEAPONMASTER_BOWS) && p_ptr->lev >= 15)
                        chance2 += 2*(bow_range(bow) - cur_dis);

                    hit = test_hit_fire(chance2 - cur_dis, armour, m_ptr->ml);
                }

                if ((bow->name1 == ART_TUBER) && (r_ptr->d_char == 'B')) /* Hack - avoid harming birds with Tuber's bow */
                {
                    msg_print("Your bow twitches in your hands!");
                    hit = FALSE;
                }

                if (p_ptr->painted_target)
                {
                    if (shoot_hack == SHOOT_BOUNCE && shoot_count > 0)
                    {
                        /* A ricochet from bouncing pebble should not reset the
                            painted target */
                    }
                    else if (!hit)
                    {
                        p_ptr->painted_target_idx = 0;
                        p_ptr->painted_target_ct = 0;
                    }
                    else if (p_ptr->painted_target_idx == c_ptr->m_idx)
                    {
                        p_ptr->painted_target_ct++;
                    }
                    else
                    {
                        p_ptr->painted_target_idx = c_ptr->m_idx;
                        p_ptr->painted_target_ct = 1;
                    }
                }

                if (melee_challenge) hit = FALSE;

                if (hit)
                {
                    bool fear = FALSE;
                    int  tdam;
                    bool ambush = FALSE;

                    if (MON_CSLEEP(m_ptr) && m_ptr->ml && !p_ptr->confused && !p_ptr->image && !p_ptr->stun)
                    {
                        if (shoot_hack == SHOOT_SNIPING || (p_ptr->pclass == CLASS_SKILLMASTER && p_ptr->ambush))
                            ambush = TRUE;
                    }
                    /* Handle unseen monster */
                    if (!visible) msg_format("The %s finds a mark.", o_name);
                    else
                    {
                        char m_name[80];
                        monster_desc(m_name, m_ptr, 0);
                        if (ambush) cmsg_format(TERM_VIOLET, "You cruelly shoot %s!", m_name);
                        else                      msg_format("The %s hits %s.", o_name, m_name);

                        if (m_ptr->ml)
                        {
                            if (!p_ptr->image) mon_track(m_ptr);
                            health_track(c_ptr->m_idx);
                        }
                    }

                    if (shoot_hack == SHOOT_NEEDLE)
                    {
                        if ((randint1(randint1(r_ptr->level/7)+5) == 1) && (!(r_ptr->flags1 & RF1_UNIQUE) || m_ptr->r_idx == MON_HAGURE2) && !(r_ptr->flags7 & RF7_UNIQUE2))
                        {
                            char m_name[80];
                            monster_desc(m_name, m_ptr, 0);
                            tdam = m_ptr->hp + 1;
                            msg_format("Your shot hit a fatal spot of %s!", m_name);
                        }
                        else tdam = 1;
                    }
                    else if (shoot_hack == SP_NEEDLE)
                    {
                        if ((randint1(randint1(r_ptr->level / (3 + p_ptr->concent)) + (8 - p_ptr->concent)) == 1)
                            && !(r_ptr->flags1 & RF1_UNIQUE) && !(r_ptr->flags7 & RF7_UNIQUE2))
                        {
                            char m_name[MAX_NLEN];
                            monster_desc(m_name, m_ptr, 0);
                            tdam = m_ptr->hp + 1;
                            msg_format("Your shot hit a fatal spot of %s!", m_name);
                        }
                        else tdam = 1;
                    }
                    else
                    {
                        critical_t crit = {0};

                        /* The Damage Calculation (Changed) */
                        tdam = damroll(dd, ds);
                        tdam += arrow.to_d;
                        if (weaponmaster_is_(WEAPONMASTER_CROSSBOWS) && p_ptr->lev >= 15)
                            tdam += 1 + p_ptr->lev/10;

                        if (shoot_hack != SHOOT_SHATTER && shoot_hack != SHOOT_ELEMENTAL)
                            tdam = tot_dam_aux_shot(&arrow, tdam, m_ptr);
                        if (ambush) tdam *= 2;
                        crit = critical_shot(arrow.weight, arrow.to_h);
                        if (crit.desc)
                        {
                            tdam = tdam * crit.mul/100 + crit.to_d;
                            msg_print(crit.desc);
                        }
                        if (p_ptr->concent) tdam = boost_concentration_damage(tdam);

                        tdam *= tmul;
                        tdam /= 100;
                        tdam += bow->to_d;

                        tdam += p_ptr->shooter_info.to_d;
                        /* End of Damage Calculation (Changed) */

                        if (weaponmaster_is_(WEAPONMASTER_CROSSBOWS))
                        {
                            if (p_ptr->lev >= 20) tdam += bow_range(bow) - cur_dis;

                            if (p_ptr->lev >= 45)
                            {
                                int mult = 100 + (m_ptr->maxhp - m_ptr->hp)*100/(2*m_ptr->maxhp);
                                tdam = tdam * mult / 100;
                            }
                        }
                        if (shoot_hack == SHOOT_RAMA) tdam *= 3;
                        //msg_format("<color:B>%d damage</color>", tdam);
                        if (tdam < 0) tdam = 0;
                        tdam = mon_damage_mod(m_ptr, tdam, FALSE);
                    }

                    if (shoot_hack == SP_EXPLODE)
                    {
                        u16b flg = (PROJECT_STOP | PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID);
                        sound(SOUND_EXPLODE); /* No explode sound - use breath fire instead */
                        project(0, ((p_ptr->concent + 1) / 2 + 1), ny, nx, tdam, GF_MISSILE, flg);
                        break;
                    }
                    if (arrow.name2 == EGO_AMMO_EXPLODING)
                    {
                        u16b flg = (PROJECT_STOP | PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID);
                        sound(SOUND_EXPLODE); /* No explode sound - use breath fire instead */
                        project(0, 3, ny, nx, tdam, GF_MISSILE, flg);
                        break;
                    }
                    if (shoot_hack == SHOOT_ELEMENTAL)
                    {
                        int rad = 0;
                        int flg = PROJECT_STOP | PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID;
                        if (weaponmaster_get_toggle() == TOGGLE_EXPLODING_BOLT)
                            rad = randint1(2+p_ptr->lev/40);
                        project(0, rad, ny, nx, tdam, GF_FIRE, flg);
                        project(0, rad, ny, nx, tdam, GF_COLD, flg);
                        project(0, rad, ny, nx, tdam, GF_ELEC, flg);
                        project(0, rad, ny, nx, tdam, GF_ACID, flg);
                        project(0, rad, ny, nx, tdam, GF_POIS, flg);
                        break;
                    }
                    if (shoot_hack == SHOOT_SHATTER)
                    {
                        int rad = 0;
                        int flg = PROJECT_STOP | PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID;
                        if (weaponmaster_get_toggle() == TOGGLE_EXPLODING_BOLT)
                            rad = randint1(2+p_ptr->lev/40);
                        tdam = tdam * (100 + p_ptr->lev*2)/100;
                        project(0, rad, ny, nx, tdam, GF_SHARDS, flg);
                        break;
                    }
                    if (weaponmaster_get_toggle() == TOGGLE_EXPLODING_BOLT)
                    {
                        int flg = PROJECT_STOP | PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID;
                        sound(SOUND_EXPLODE); /* No explode sound - use breath fire instead */
                        project(0, randint1(2+p_ptr->lev/40), ny, nx, tdam, GF_MISSILE, flg);
                        break;
                    }
                    if (shoot_hack == SP_HOLYNESS)
                    {
                        cave[ny][nx].info |= (CAVE_GLOW);
                        note_spot(ny, nx);
                        lite_spot(ny, nx);
                    }
                    if (p_ptr->stun)
                        tdam -= tdam * MIN(100, p_ptr->stun) / 150;
                    if (mon_take_hit(c_ptr->m_idx, tdam, DAM_TYPE_ARCHERY, &fear, NULL))
                    {
                        /* Dead monster ... abort firing additional shots */
                        i = num_shots;
                        p_ptr->painted_target_idx = 0;
                        p_ptr->painted_target_ct = 0;
                    }
                    /* No death */
                    else
                    {
                        bool anger = TRUE;

                        message_pain(c_ptr->m_idx, tdam);

                        if (shoot_hack == SHOOT_TRANQUILIZE)
                        {
                            if (MON_CSLEEP(m_ptr))
                            {
                                if (!one_in_(5))
                                    anger = FALSE;
                            }
                            else if (one_in_(3))
                            {
                                char m_name[80];
                                monster_desc(m_name, m_ptr, 0);
                                msg_format("%^s is put to sleep!", m_name);
                                set_monster_csleep(c_ptr->m_idx, 500);
                                anger = FALSE;
                            }

                            /* mon_take_hit() in xtra2.c hacked to not auto wakeup for SHOOT_TRANQUILIZE */
                            if (anger)
                                (void)set_monster_csleep(c_ptr->m_idx, 0);
                        }

                        if (anger && tdam > 0 && m_ptr->cdis > 1 && allow_ticked_off(r_ptr))
                        {
                            if (!mut_present(MUT_PEERLESS_SNIPER) && !p_ptr->tim_stealthy_snipe)
                                mon_anger_shoot(m_ptr, tdam);
                        }

                        if (anger && tdam > 0) anger_monster(m_ptr);

                        /* Artifact arrows stick to target. Note, we now do this
                           after hurting/angering the monster since Cupid's Arrow
                           might charm the target, while anger_monster() would set
                           it back to being hostile. */
                        if (object_is_fixed_artifact(&arrow))
                        {
                            char m_name[80];
                            monster_desc(m_name, m_ptr, 0);

                            stick_to = ((one_in_(2)) && (arrow.name1 != ART_BRAHMA));

                            /* If Cupid's Arrow charms the monster,
                               having the arrow stick is highly annoying since
                               you can't command your pets to drop objects they
                               are carrying. */
                            if (arrow.name1 == ART_CUPIDS_ARROW && !(r_ptr->flags1 & RF1_UNIQUE))
                            {
                                if (!mon_save_p(m_ptr->r_idx, A_CHR))
                                {
                                    if (!mon_save_p(m_ptr->r_idx, A_CHR))
                                    {
                                        if (!is_pet(m_ptr))
                                        {
                                            set_pet(m_ptr);
                                            msg_format("%^s is charmed!", m_name);
                                            stick_to = FALSE;
                                        }
                                        else if (!is_friendly(m_ptr))
                                        {
                                            set_friendly_ingame(m_ptr);
                                            msg_format("%^s suddenly becomes friendly.", m_name);
                                            stick_to = FALSE;
                                        }
                                    }
                                    else if (!is_pet(m_ptr) && !is_friendly(m_ptr))
                                    {
                                        set_friendly_ingame(m_ptr);
                                        msg_format("%^s suddenly becomes friendly.", m_name);
                                        stick_to = FALSE;
                                    }
                                }
                            }

                            if (stick_to) msg_format("%^s sticks to %s!",o_name, m_name);
                        }

                        if (fear && m_ptr->ml)
                        {
                            char m_name[80];
                            sound(SOUND_FLEE);
                            monster_desc(m_name, m_ptr, 0);
                            msg_format("%^s flees in terror!", m_name);
                        }

                        set_target(m_ptr, py, px);

                        if (shoot_hack == SP_RUSH || shoot_hack == SHOOT_KNOCKBACK)
                        {
                            int n = randint1(5) + 3;
                            int m_idx = c_ptr->m_idx;

                            for ( ; cur_dis <= tdis; )
                            {
                                int ox = nx;
                                int oy = ny;

                                if (!n) break;

                                /* Calculate the new location (see "project()") */
                                mmove2(&ny, &nx, sy, sx, ty, tx);

                                /* Stopped by wilderness boundary */
                                if (!in_bounds2(ny, nx)) break;

                                /* Stopped by walls/doors */
                                if (!player_can_enter(cave[ny][nx].feat, 0)) break;

                                /* Stopped by monsters */
                                if (!cave_empty_bold(ny, nx)) break;

                                cave[ny][nx].m_idx = m_idx;
                                cave[oy][ox].m_idx = 0;

                                m_ptr->fx = nx;
                                m_ptr->fy = ny;

                                /* Update the monster (new location) */
                                update_mon(c_ptr->m_idx, TRUE);

                                lite_spot(ny, nx);
                                lite_spot(oy, ox);

                                Term_fresh();
                                Term_xtra(TERM_XTRA_DELAY, msec);

                                x = nx;
                                y = ny;
                                cur_dis++;
                                n--;
                            }
                        }
                    }
                }
                else
                {
                    char m_name[80];
                    monster_desc(m_name, m_ptr, 0);
                    msg_format("The %s misses %s.", o_name, m_name);
                }

                /* The following effects (piercing and bouncing) should
                   not take place when artifact ammo has stuck to a unique! */
                if (!stick_to)
                {
                    if (shoot_hack == SP_PIERCE)
                    {
                        if(p_ptr->concent < 1) break;
                        p_ptr->concent--;
                        continue;
                    }

                    if (hit && shoot_count < 5 && (shoot_hack == SHOOT_PIERCE || weaponmaster_get_toggle() == TOGGLE_PIERCING_ARROW))
                    {
                        /*  @.....+......o
                            Is there a ghost standing on the door? Double check the terrain
                            before allowing a pierce to continue towards that orc.
                        */
                        if (!cave_have_flag_bold(ny, nx, FF_PROJECT)) break;
                        chance -= 20 * BTH_PLUS_ADJ;
                        shoot_count++;
                        continue;
                    }

                    if (shoot_hack == SHOOT_BOUNCE)
                    {
                        int dir = randint1(9);
                        if (dir != 5)
                        {
                            /* For now, only one bounce ... Consider allowing a chance
                               of multiple bounces (e.g. one_in_(shoot_count)) or just
                               letting the pebble bounce until it fails to hit a monster */
                            shoot_count++;
                            if (shoot_count <= 5)
                            {
                                tx = x + 99 * ddx[dir];
                                ty = y + 99 * ddy[dir];
                                do_cmd_fire_aux2(bow, arrows, x, y, tx, ty);
                                return;
                            }
                        }
                    }
                }

                /* Stop looking */
                break;
            }
        }

        /* Chance of breakage (during attacks) */
        break_chance = (hit_body ? breakage_chance(&arrow) : 0);
        if (shoot_hack == SHOOT_DISINTEGRATE) break_chance = 100;

        if (return_ammo)
        {
            if (disturb_minor) msg_format("The %s returns to your pack.", o_name);
        }
        else if (stick_to)
        {
            int m_idx = cave[y][x].m_idx;
            monster_type *m_ptr = &m_list[m_idx];
            int o_idx = o_pop();
            obj_ptr o_ptr;

            if (!o_idx)
            {
                msg_format("The %s has gone somewhere else.", o_name);
                if (object_is_fixed_artifact(&arrow))
                    a_info[arrow.name1].generated = FALSE;
                if (random_artifacts && arrow.name3)
                    a_info[arrow.name3].generated = FALSE;
                return;
            }

            o_ptr = &o_list[o_idx];
            *o_ptr = arrow;

            o_ptr->marked &= (OM_TOUCHED | OM_COUNTED | OM_EFFECT_COUNTED | OM_EGO_COUNTED | OM_ART_COUNTED);
            o_ptr->loc.where = INV_FLOOR;
            o_ptr->loc.y = o_ptr->loc.x = 0;
            o_ptr->loc.slot = o_idx;
            o_ptr->held_m_idx = m_idx;
            o_ptr->next_o_idx = m_ptr->hold_o_idx;
            m_ptr->hold_o_idx = o_idx;
        }
        else if (cave_have_flag_bold(y, x, FF_PROJECT))
        {
            /* Drop (or break) near that location */
            drop_near(&arrow, break_chance, y, x);
        }
        else
        {
            /* Drop (or break) near that location */
            drop_near(&arrow, break_chance, prev_y, prev_x);
        }

    /* Sniper - Repeat shooting when double shots */
    }

    /* Sniper - Lose concentration after any shot */
    if (p_ptr->concent) reset_concentration(FALSE);

    /* Check for easy tiring */
    p_inc_fatigue(MUT_EASY_TIRING2, 7500 / NUM_SHOTS);
    check_muscle_sprains(250, "You feel a sudden sharp pain running down from your right shoulder!");
}


bool do_cmd_fire(void)
{
    bool         result = FALSE;
    obj_prompt_t prompt = {0};
    int          slot = equip_find_first(object_is_bow);
    obj_ptr      bow = NULL;

    if (!slot)
    {
        msg_print("You have nothing to fire with.");
        flush();
        return FALSE;
    }

    if (prace_is_(MIMIC_MIST))
    {
        msg_print("You cannot shoot while incorporeal.");
        flush();
        return FALSE;
    }

    bow = equip_obj(slot);

    if (bow->sval == SV_CRIMSON || bow->sval == SV_RAILGUN)
    {
        msg_print("You should activate your Gun instead.");
        flush();
        return FALSE;
    }

    if (bow->sval == SV_HARP || bow->sval == SV_FLUTE)
    {
        msg_print("You play a soothing melody, but not much else happens.");
        flush();
        return FALSE;
    }

    if (p_ptr->special_defense & KATA_MUSOU) set_action(ACTION_NONE);

    prompt.prompt = "Fire which item?";
    prompt.error = "You have nothing to fire.";
    prompt.filter = obj_can_shoot;
    prompt.where[0] = INV_QUIVER;
    prompt.where[1] = INV_PACK;
    prompt.where[2] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    result = do_cmd_fire_aux1(bow, prompt.obj);
    if (result && p_ptr->pclass == CLASS_SNIPER)
    {
        if (shoot_hack == SP_AWAY)
            teleport_player(10 + (p_ptr->concent * 2), 0L);
        if (shoot_hack == SP_FINAL)
        {
            msg_print("You experience a powerful recoil!");
            set_slow(p_ptr->slow + randint0(7) + 7, FALSE);
            set_stun(p_ptr->stun + randint1(25), FALSE);
        }
    }

    return result;
}


/*
 * Hack: travel command
 */
#define TRAVEL_UNABLE 8192

static int flow_head = 0;
static int flow_tail = 0;
static s16b temp2_x[MAX_SHORT];
static s16b temp2_y[MAX_SHORT];

/* Hack: forget the "flow" information */
void forget_travel_flow(void)
{
    /* Check the entire level */
    for (int y = 0; y < cur_hgt; y++)
        for (int x = 0; x < cur_wid; x++)
            travel.cost[y][x] = TRAVEL_UNABLE; // Forget the old data
}

static float _travel_flow_penalty(feature_type *f_ptr)
{
    if (have_flag(f_ptr->flags, FF_LAVA) && !elemental_is_(ELEMENTAL_FIRE) && res_pct(RES_FIRE) < 100)
    {
        int penalty = (have_flag(f_ptr->flags, FF_DEEP)) ? 16 : 2;
        if (res_pct(RES_FIRE) <= 50) penalty *= 4;
        if (p_ptr->levitation) penalty /= 2;
        return penalty;
    }
    else if (have_flag(f_ptr->flags, FF_ACID))
    {
        int penalty = (have_flag(f_ptr->flags, FF_DEEP)) ? 12 : 6;
        if (res_pct(RES_ACID) <= 50) penalty *= 4;
        if (p_ptr->levitation) penalty /= 2;
        return penalty;
    }
    else if (!p_ptr->levitation && !p_ptr->can_swim && have_flag(f_ptr->flags, FF_WATER) &&
             have_flag(f_ptr->flags, FF_DEEP) && !elemental_is_(ELEMENTAL_WATER) && py_total_weight() > weight_limit())
    {
        return 4;
    }
    else if (have_flag(f_ptr->flags, FF_WATER) && !elemental_is_(ELEMENTAL_WATER)) return 0.2f;
    else if (have_flag(f_ptr->flags, FF_TREE)) return 0.9f;
    else if (have_flag(f_ptr->flags, FF_HAS_GOLD) || have_flag(f_ptr->flags, FF_HAS_ITEM)) return -0.2f;
    else if (f_ptr->mimic == feat_floor) return -0.1f; // Prefer to walk on roads
    else return 0.0f;
}

static bool travel_flow_aux(int y, int x, float n, bool wall)
{
    cave_type *c_ptr = &cave[y][x];

    feature_type *f_ptr = &f_info[get_feat_mimic(c_ptr)];
    int old_head = flow_head;

    while (n >= TRAVEL_UNABLE) n -= TRAVEL_UNABLE;

    /* Ignore out of bounds or undiscovered terrain */
    if (!in_bounds(y, x)) return wall;
    if (!(c_ptr->info & CAVE_AWARE)) return wall;

    /* Don't travel thru traps ... code will attempt a disarm, but this
     * can be dangerous for some players. Often, there is an alternate
     * route around the trap anyway ... */
    if (is_known_trap(c_ptr)) return wall;

    n += _travel_flow_penalty(f_ptr);

    /* Ignore "pre-stamped" entries */
    if (travel.cost[y][x] > TRAVEL_UNABLE || (travel.cost[y][x] < TRAVEL_UNABLE && travel.cost[y][x] <= n)) return wall;

    /* Ignore "walls" and "rubble" (include "secret doors") */
    if (have_flag(f_ptr->flags, FF_WALL) ||
        have_flag(f_ptr->flags, FF_CAN_DIG) ||
        is_hidden_door(c_ptr) ||
        (!have_flag(f_ptr->flags, FF_MOVE) && have_flag(f_ptr->flags, FF_CAN_FLY) && !p_ptr->levitation))
    {
        if (!wall) return wall;
    }
    else
    {
        wall = FALSE;
    }

    /* Save the flow cost */
    travel.cost[y][x] = n;
    if (wall) travel.cost[y][x] += TRAVEL_UNABLE;

    /* Enqueue that entry */
    temp2_y[flow_head] = y;
    temp2_x[flow_head] = x;

    /* Advance the queue */
    if (++flow_head == MAX_SHORT) flow_head = 0;

    /* Hack -- notice overflow by forgetting new entry */
    if (flow_head == flow_tail) flow_head = old_head;

    return wall;
}


static void travel_flow(int ty, int tx)
{
    feature_type *f_ptr = &f_info[cave[ty][tx].feat];
    flow_head = flow_tail = 0; // Reset the "queue"

    bool wall = !have_flag(f_ptr->flags, FF_MOVE);

    /* Add the target destination grid to the queue */
    wall = travel_flow_aux(ty, tx, 0, wall);

    /* Now process the queue */
    while (flow_head != flow_tail)
    {
        /* Extract the next entry */
        int y = temp2_y[flow_tail];
        int x = temp2_x[flow_tail];

        /* Forget that entry */
        if (++flow_tail == MAX_SHORT) flow_tail = 0;

        /* Add the "children" if legal */
        for (int d = 0; d < 8; d++)
        {
            float step_cost = (d & 1) ? 1.2 : 1.0; // Diagonals cost a bit more
            wall = travel_flow_aux(y + ddy_cdd[d], x + ddx_cdd[d], travel.cost[y][x] + step_cost, wall);
        }
    }

    /* Forget the flow info */
    flow_head = flow_tail = 0;
}

bool can_travel(int y, int x)
{
    assert(in_bounds2(y, x));
    if (y == py && x == px) return FALSE;

    cave_type *c_ptr = &cave[y][x];

    if(!(c_ptr->info & CAVE_MARK))             return FALSE; // Don't travel to unknown grids
    if(cave_have_flag_grid(c_ptr, FF_WALL))    return FALSE; // Don't travel to walls
    if(cave_have_flag_grid(c_ptr, FF_CAN_DIG)) return FALSE; // Don't travel to rubble
    if(is_hidden_door(c_ptr))                  return FALSE; // Don't travel to hidden doors
    if(is_known_trap(c_ptr))                   return FALSE; // Don't travel to known traps

    return TRUE;
}

void travel_begin(int mode, int x, int y)
{
    assert(in_bounds2(y, x));

    travel.mode = mode;
    travel.run = 0;
    travel.aborted = FALSE;

    if (y == py && x == px)
    {
        /* Shut up already ... perhaps we are being called from wilderness_move_player, rather
        than from the top level. It turns out that the Museum in Outpost is located on a scroll
        boundary, and the scroll fires on the last move of the travel flow, but is processed
        before travel_step checks that we are finished. */
        msg_print("You are already there!");
        travel_cancel();
        return;
    }

    if (!can_travel(y, x))
    {
        msg_print("You cannot travel there!");
        travel_cancel();
        return;
    }

    travel.y = y;
    travel.x = x;

    forget_travel_flow();
    travel_flow(y, x);

    // Travel up to 255 steps
    // Autoexplore starts with "one step already taken"
    // This first step is "added back" only after the initial travel_begin() call
    // This is so we print "Autoexplore canceled" only in response to the initial 'x' keypress
    travel.run = (mode == TRAVEL_MODE_AUTOEXPLORE) ? 254 : 255;
    travel.dir = 0;

    /* Decides first direction */
    int dy = abs(py - y);
    int dx = abs(px - x);

    int sy = (y == py || dy < dx) ? 0 : (y > py) ? 1 : -1;
    int sx = (x == px || dx < dy) ? 0 : (x > px) ? 1 : -1;

    for (int i = 1; i <= 9; i++)
        if (sx == ddx[i] && sy == ddy[i]) { travel.dir = i; break; }
}

void travel_wilderness_scroll(int new_x, int new_y)
{
    bool was_travelling = (travel.run != 0);
    if (new_x == px && new_y == py)
    {
        /* Don't begin a new travel if we are just going
         * to end it. Call stack is something like:
         *   travel_wilderness_scroll
         *   wilderness_move_player
         *   move_player_effect
         *   travel_step
         * When we return to travel_step, it will call
         * travel_end for us. If we call travel_end, then
         * we (might) begin a new travel command, and unwinding
         * to travel_step erroneously (but harmlessly)
         * marks a step as taken on the new route. */
        travel.x = px;
        travel.y = py;
    }
    /* Careful: Travel might cause a scroll that sends the destination
     * out of bounds. For example: Telmora and The Sand Pits quest. Travel
     * from the Manor square to the quest entrance to reproduce. */
    else if (in_bounds2(new_y, new_x))
    {
        if (was_travelling) travel_begin(travel.mode, new_x, new_y);
        else
        {
            travel.x = new_x;
            travel.y = new_y;
        }
    }
    else
    {
        travel_cancel();
        forget_travel_flow();
        if (was_travelling) msg_print("<color:v>Oops!</color> The location you were travelling towards has scrolled off the screen.");
        travel.x = 0;
        travel.y = 0;
    }
}

void travel_cancel(void)
{
    travel.run = 0;
    /* Don't twiddle with the mode here ... If you examine travel_step
     * you will see it manually undoes the effects of disturb() during
     * the move_player_effect(). It does this by remembering travel.run
     * and resetting it when all is done. So, if we were to clear the
     * mode here, then we would abort an intermediate stage in a travel
     * sequence (e.g. TRAVEL_MODE_AUTOPICK). It's OK to leave the mode
     * set after the travel is legitimately completed since nobody can
     * start a new travel without calling travel_begin(mode, ...).
     *
     * BTW, the autopicker is triggering the disturb during travel.
     * In our case, we are travelling just so the autopicker can do
     * its thing, and aborting would be a gross error.
     *
     * travel.mode = TRAVEL_MODE_NORMAL;*/
}

void travel_cancel_fully(void)
{
    travel.run = 0;
    travel.aborted = FALSE;
    travel.mode = TRAVEL_MODE_NORMAL;
}

void travel_end(void)
{
  travel.run = 0;
  travel.aborted = FALSE;
  
  if(p_ptr->confused || p_ptr->move_random) travel.mode = TRAVEL_MODE_NORMAL;
  
  if(travel.mode == TRAVEL_MODE_AUTOEXPLORE){
    if (_travel_continue(FALSE)) {
      if (travel.y == py && travel.x == px) travel_cancel();
      else return;
    }
    else msg_print("Exploration complete!");
  } else if ((travel.mode == TRAVEL_MODE_AMMO || travel.mode == TRAVEL_MODE_AUTOPICK)) {
    if(_travel_next_obj(travel.mode)) {
      if (travel.y == py && travel.x == px) travel_cancel();
      else return;
    }
  }
  
  travel.mode = TRAVEL_MODE_NORMAL;
  if (center_player && !center_running) viewport_recenter();
}

void do_cmd_travel(void)
{
    int x, y;
    if (tgt_pt(&x, &y, -1)) travel_begin(TRAVEL_MODE_NORMAL, x, y);
}

void do_cmd_get_nearest(void)
{
    int old_y = travel.y;
    int old_x = travel.x;
    int by = 0, bx = 0;
    int _itms = 0;
    float best = TRAVEL_UNABLE;
    travel_cancel_fully();
    for (int i = 0; i < max_o_idx; i++)
    {
        object_type *o_ptr = &o_list[i];

        if (!o_ptr->k_idx) continue;
        if (!(o_ptr->marked & OM_FOUND)) continue;
        if (o_ptr->tval == TV_GOLD) continue;
        if (obj_is_known(o_ptr)) continue;
        if ((o_ptr->ident & IDENT_SENSE) &&
            (!obj_is_device(o_ptr)) &&
            (o_ptr->feeling != FEEL_EXCELLENT) &&
            (o_ptr->feeling != FEEL_SPECIAL) &&
            (o_ptr->feeling != FEEL_AWFUL) &&
            (o_ptr->feeling != FEEL_TERRIBLE) &&
            (o_ptr->feeling != FEEL_ENCHANTED)) continue;
        if (!in_bounds(o_ptr->loc.y, o_ptr->loc.x)) continue;
        _itms++;
        if ((by) && (MAX(ABS(py - o_ptr->loc.y), ABS(px - o_ptr->loc.x)) >= best)) continue;
        if ((o_ptr->loc.y == py) && (o_ptr->loc.x == px)) continue;
        if ((o_ptr->loc.y == by) && (o_ptr->loc.x == bx)) continue;

        if ((max_autopick > 1) && (!no_mogaminator))
        {
            int auto_pick_idx = is_autopick(o_ptr);
            if (auto_pick_idx < 0 || !(autopick_list[auto_pick_idx].action & (DO_AUTOPICK | DO_QUERY_AUTOPICK)))
            {
                _itms--;
                continue;
            }
        }
        forget_travel_flow();
        travel_flow(o_ptr->loc.y, o_ptr->loc.x);
        float cost = travel.cost[py][px];
        if (cost < best)
        {
            best = cost;
            by = o_ptr->loc.y;
            bx = o_ptr->loc.x;
        }
    }
    forget_travel_flow();
    if (best < TRAVEL_UNABLE)
    {
        travel_begin(TRAVEL_MODE_NORMAL, bx, by);
        return;
    }
    else
    {
        travel.y = old_y;
        travel.x = old_x;
        if (!_itms)                     msg_print("You are not aware of any interesting unidentified items.");
        else if (best >= TRAVEL_UNABLE) msg_print("You cannot find a route to any interesting object.");
    }
}

