/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercice 05
*/

#include <stdlib.h>
#include <stdarg.h>
#include "raise.h"
#include "list.h"
#include "new.h"

typedef struct Node_s {
    Object          *data;
    struct Node_s   *next;
} Node;

typedef struct {
    Container   base;
    Class       *_type;
    size_t      _size;
    Node        *head;
} ListClass;

/*
* Function to create a new List object
*/
static void List_ctor(ListClass *this, va_list *args) {
    this->_type = va_arg(*args, Class *);
    this->_size = 0;
    this->head = NULL;
}

/*
* Function to delete a node
*/
static void delete_node(Node *node) {
    delete(node->data);
    free(node);
}

/*
* Function to delete a List object
*/
static void List_dtor(ListClass *this) {
    Node *cur = this->head;
    Node *next;

    while (cur) {
        next = cur->next;
        delete_node(cur);
        cur = next;
    }
    this->head = NULL;
    this->_size = 0;
}

/*
* Function to get the length of the List
*/
static size_t List_len(ListClass *this) {
    return this->_size;
}

/*
* Function to create a new node
*/
static Node *create_node(Object *data) {
    Node *newNode = malloc(sizeof(Node));
    if (!newNode)
        raise("Memory allocation failed");
    newNode->data = data;
    newNode->next = NULL;
    return newNode;
}

/*
* Function to add an element at the front of the List
*/
static void List_addAtFront(ListClass *this, Object *data) {
    if (!data)
        raise("Invalid data");
    Node *newNode = create_node(data);
    newNode->next = this->head;
    this->head = newNode;
    this->_size++;
}

/*
* Function to add an element at the back of the List
*/
static void List_addAtBack(ListClass *this, Object *data) {
    if (!data)
        raise("Invalid data");
    Node *newNode = create_node(data);
    Node *cur = this->head;

    if (!cur) {
        this->head = newNode;
    } else {
        while (cur->next)
            cur = cur->next;
        cur->next = newNode;
    }
    this->_size++;
}

/*
* Function to add an element at a specific position in the List
*/
static void List_addAtPosition(ListClass *this, Object *data, size_t position) {
    if (!data)
        raise("Invalid data");
    if (position == 0) {
        List_addAtFront(this, data);
    } else if (position >= this->_size) {
        List_addAtBack(this, data);
    } else {
        Node *newNode = create_node(data);
        Node *cur = this->head;
        for (size_t i = 0; i < position - 1; i++) {
            cur = cur->next;
        }
        newNode->next = cur->next;
        cur->next = newNode;
        this->_size++;
    }
}

/*
* Function to get the element at the front of the List
*/
static const Object *List_getFront(ListClass *this) {
    if (!this->head)
        return NULL;
    return this->head->data;
}

/*
* Function to get the element at the back of the List
*/
static const Object *List_getBack(ListClass *this) {
    Node *cur = this->head;

    if (!cur)
        return NULL;
    while (cur->next)
        cur = cur->next;
    return cur->data;
}

/*
* Function to get the element at a specific position in the List
*/
static const Object *List_getAtPosition(ListClass *this, size_t position) {
    if (position >= this->_size)
        raise("Invalid position");
    Node *cur = this->head;
    for (size_t i = 0; i < position && cur->next; i++) {
        cur = cur->next;
    }
    return cur->data;
}

/*
* Function to remove the element at the front of the List
*/
static void List_popFront(ListClass *this) {
    if (!this->head)
        return;
    Node *cur = this->head;
    this->head = cur->next;
    delete_node(cur);
    this->_size--;
}

/*
* Function to remove the element at the back of the List
*/
static void List_popBack(ListClass *this) {
    if (!this->head)
        return;
    Node *cur = this->head;
    Node *prev = NULL;

    if (!cur->next) {
        delete_node(cur);
        this->head = NULL;
    } else {
        while (cur->next) {
            prev = cur;
            cur = cur->next;
        }
        prev->next = NULL;
        delete_node(cur);
    }
    this->_size--;
}

/*
* Function to remove the element at a specific position in the List
*/
static void List_popAtPosition(ListClass *this, size_t position) {
    if (position >= this->_size)
        raise("Invalid position");
    if (position == 0) {
        List_popFront(this);
    } else if (position >= this->_size - 1) {
        List_popBack(this);
    } else {
        Node *cur = this->head;
        Node *prev = NULL;
        for (size_t i = 0; i < position; i++) {
            prev = cur;
            cur = cur->next;
        }
        prev->next = cur->next;
        delete_node(cur);
        this->_size--;
    }
}

/*
* Function to print the elements of the List
*/
static void List_dump(ListClass *this) {
    Node *cur = this->head;

    while (cur) {
        printf("%s\n", ((Class *)cur->data)->__str__(cur->data));
        cur = cur->next;
    }
}

/*
* Define the List class and its methods
*/
static const ListClass _descr = {
    {   /* Container struct */
        {   /* Class struct */
            .__size__ = sizeof(ListClass),
            .__name__ = "List",
            .__ctor__ = (ctor_t)&List_ctor,
            .__dtor__ = (dtor_t)&List_dtor,
            .__str__ = NULL,
            .__add__ = NULL,
            .__sub__ = NULL,
            .__mul__ = NULL,
            .__div__ = NULL,
            .__eq__ = NULL,
            .__gt__ = NULL,
            .__lt__ = NULL,
        },
        .__len__ = (len_t)&List_len,
        .__addAtFront__ = (addAtFront_t)&List_addAtFront,
        .__addAtBack__ = (addAtBack_t)&List_addAtBack,
        .__addAtPosition__ = (addAtPosition_t)&List_addAtPosition,
        .__getFront__ = (getFront_t)&List_getFront,
        .__getBack__ = (getBack_t)&List_getBack,
        .__getAtPosition__ = (getAtPosition_t)&List_getAtPosition,
        .__popFront__ = (popFront_t)&List_popFront,
        .__popBack__ = (popBack_t)&List_popBack,
        .__popAtPosition__ = (popAtPosition_t)&List_popAtPosition,
        .__dump__ = (dump_t)&List_dump,
    },
    ._type = NULL,
    ._size = 0,
    .head = NULL
};

const Class *List = (const Class *)&_descr;
