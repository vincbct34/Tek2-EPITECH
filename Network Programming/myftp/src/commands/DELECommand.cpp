/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** CDUPCommand.cpp
*/

#include "DELECommand.hpp"
#include "SocketActions.hpp"

std::string DELECommand::execute(const std::string& args, ClientSession& session) {
    (void)session;

    std::string trimmedArgs = args;
    trimmedArgs.erase(trimmedArgs.find_last_not_of(" \n\r\t") + 1);

    if (SocketActions::removeFile(trimmedArgs.c_str()) == 0) {
        return "250 File successfully deleted.\r\n";
    }

    return "550 Failed to delete file.\r\n";
}
