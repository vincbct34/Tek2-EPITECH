/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercice 05
*/

#pragma once

#include "object.h"
#include "iterator.h"
#include "raise.h"

typedef struct Container_s Container;

typedef Iterator *(*iter_t)(Container *this);
typedef size_t (*len_t)(Container *this);
typedef Object *(*getitem_t)(Container *this, ...);
typedef void (*setitem_t)(Container *this, ...);
typedef void (*addAtFront_t)(Container *this, ...);
typedef void (*addAtBack_t)(Container *this, ...);
typedef void (*addAtPosition_t)(Container *this, ...);
typedef Object *(*getFront_t)(Container *this);
typedef Object *(*getBack_t)(Container *this);
typedef Object *(*getAtPosition_t)(Container *this, ...);
typedef void (*popFront_t)(Container *this);
typedef void (*popBack_t)(Container *this);
typedef void (*popAtPosition_t)(Container *this, size_t position);
typedef void (*dump_t)(Container *this);

struct Container_s {
    Class       base;
    len_t       __len__;
    iter_t      __begin__;
    iter_t      __end__;
    getitem_t   __getitem__;
    setitem_t   __setitem__;
    addAtFront_t __addAtFront__;
    addAtBack_t __addAtBack__;
    addAtPosition_t __addAtPosition__;
    getFront_t __getFront__;
    getBack_t __getBack__;
    getAtPosition_t __getAtPosition__;
    popFront_t __popFront__;
    popBack_t __popBack__;
    popAtPosition_t __popAtPosition__;
    dump_t __dump__;
};

#define len(c)          ((Container *)c)->__len__(c)
#define begin(c)        ((Container *)c)->__begin__(c)
#define end(c)          ((Container *)c)->__end__(c)
#define getitem(c, ...) ((Container *)c)->__getitem__(c, __VA_ARGS__)
#define setitem(c, ...) ((Container *)c)->__setitem__(c, __VA_ARGS__)
#define addAtFront(c, ...) ((Container *)c)->__addAtFront__(c, __VA_ARGS__)
#define addAtBack(c, ...) ((Container *)c)->__addAtBack__(c, __VA_ARGS__)
#define addAtPosition(c, ...) ((Container *)c)->__addAtPosition__(c, __VA_ARGS__)
#define getFront(c) ((Container *)c)->__getFront__(c)
#define getBack(c) ((Container *)c)->__getBack__(c)
#define getAtPosition(c, ...) ((Container *)c)->__getAtPosition__(c, __VA_ARGS__)
#define popFront(c) ((Container *)c)->__popFront__(c)
#define popBack(c) ((Container *)c)->__popBack__(c)
#define popAtPosition(c, ...) ((Container *)c)->__popAtPosition__(c, __VA_ARGS__)
#define dump(c) ((Container *)c)->__dump__(c)
