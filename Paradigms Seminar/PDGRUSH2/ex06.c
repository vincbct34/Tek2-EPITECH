// /*
// ** EPITECH PROJECT, 2025
// ** Paradigms Seminar
// ** File description:
// ** Exercise 06
// */

// #include <stdio.h>
// #include "new.h"
// #include "list.h"

// extern const Class *Int;

// int main(void)
// {
//     // Create a linked list of integers
//     Object *list = new(List, Int);

//     // Add elements to the list
//     printf("Adding 1 at front\n");
//     addAtFront(list, new(Int, 1));
//     printf("Adding 2 at back\n");
//     addAtBack(list, new(Int, 2));
//     printf("Adding 3 at position 1\n");
//     addAtPosition(list, new(Int, 3), 1);
//     printf("Adding 4 at position 0\n");
//     addAtPosition(list, new(Int, 4), 0);
//     printf("Adding 5 at position 10 (should add at the back)\n");
//     addAtPosition(list, new(Int, 5), 10);

//     // Print the list
//     printf("List after adding elements:\n");
//     dump(list);

//     // Get elements from the list
//     printf("Front: %s\n", str(getFront(list)));
//     printf("Back: %s\n", str(getBack(list)));
//     printf("Element at position 1: %s\n", str(getAtPosition(list, 1)));

//     // Remove elements from the list
//     printf("Removing front element\n");
//     popFront(list);
//     printf("Removing back element\n");
//     popBack(list);
//     printf("Removing element at position 1\n");
//     popAtPosition(list, 1);

//     // Print the list again
//     printf("List after removing elements:\n");
//     dump(list);

//     // Clean up
//     delete(list);

//     return 0;
// }
