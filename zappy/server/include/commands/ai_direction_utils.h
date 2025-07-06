/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Direction calculation utilities header
*/

#ifndef AI_DIRECTION_UTILS_H_
    #define AI_DIRECTION_UTILS_H_

/**
 * @brief Represents a pair of positions (source and destination).
 */
typedef struct {
    int src_x;
    int src_y;
    int dst_x;
    int dst_y;
} position_pair_t;

/**
 * @brief Represents the dimensions of a map.
 */
typedef struct {
    int width;
    int height;
} map_dimensions_t;

/**
 * @brief Represents the position information of an entity.
 */
typedef struct {
    int x;
    int y;
    int orientation;
} position_info_t;

int calculate_direction(position_pair_t *positions, map_dimensions_t *map,
    int listener_orientation);
void calculate_new_position(position_info_t *pos, map_dimensions_t *map);

#endif /* !AI_DIRECTION_UTILS_H_ */
