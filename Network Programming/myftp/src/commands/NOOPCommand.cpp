/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** NOOPCommand.cpp
*/

#include "NOOPCommand.hpp"

std::string NOOPCommand::execute(const std::string& args, ClientSession& session) {
    (void)args;
    (void)session;

    return "200 Command okay.\r\n";
}
