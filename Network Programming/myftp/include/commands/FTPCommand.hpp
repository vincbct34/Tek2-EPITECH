/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** FTPCommand.hpp
*/

#pragma once

#include <string>

class ClientSession;

class FTPCommand {
    public:
        virtual ~FTPCommand() = default;

        virtual std::string execute(const std::string& args, ClientSession& session) = 0;
};
