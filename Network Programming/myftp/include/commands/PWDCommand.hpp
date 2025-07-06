/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** PWDCommand.hpp
*/

#pragma once

#include <unistd.h>
#include <limits.h>

#include "SocketActions.hpp"
#include "FTPCommand.hpp"

class PWDCommand : public FTPCommand {
public:
    std::string execute(const std::string& args, ClientSession& session) override;
};
