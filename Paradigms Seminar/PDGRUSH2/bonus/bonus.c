/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Bonus
*/

#include <stdio.h>
#include "new.h"
#include "string.h"

int main(void)
{
    // Create two strings and concatenate them
    Object *s1 = new(String, "Hello");
    Object *s2 = new(String, "World");
    Object *s3 = addition(s1, s2);

    // Get the strings
    char *str1 = str(s1);
    char *str2 = str(s2);
    char *str3 = str(s3);

    // Print the strings
    printf("%s\n", str1);
    printf("%s\n", str2);
    printf("%s\n", str3);

    // Clean up
    delete(s1);
    delete(s2);
    delete(s3);
    free(str1);
    free(str2);
    free(str3);

    return 0;
}
