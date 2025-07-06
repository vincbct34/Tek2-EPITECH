/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Resource utilities header for AI commands
*/

#ifndef RESOURCE_UTILS_H_
    #define RESOURCE_UTILS_H_

/**
 * @brief Convert resource name string to resource type enum
 * @param resource_name The name of the resource
 * @return The resource type enum value, or -1 if invalid
 */
int get_resource_type_from_string(const char *resource_name);

#endif /* !RESOURCE_UTILS_H_ */
