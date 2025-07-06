/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** CWDCommand.cpp
*/

#include "CWDCommand.hpp"

std::string CWDCommand::execute(const std::string& args, ClientSession& session) {
    (void)session;

    std::string trimmedArgs = args;
    trimmedArgs.erase(trimmedArgs.find_last_not_of(" \n\r\t") + 1);

    if (SocketActions::directoryNavigation(trimmedArgs.c_str()) == 0) {
        return "250 Directory successfully changed.\r\n";
    }

    return "550 Failed to change directory.\r\n";
}
