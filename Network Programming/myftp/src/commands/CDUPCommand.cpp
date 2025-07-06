/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** CDUPCommand.cpp
*/

#include "CDUPCommand.hpp"
#include "SocketActions.hpp"

std::string CDUPCommand::execute(const std::string& args, ClientSession& session) {
    (void)args;
    (void)session;

    if (SocketActions::directoryNavigation("..") == 0) {
        return "250 Directory successfully changed to parent.\r\n";
    }

    return "550 Failed to change directory to parent.\r\n";
}
