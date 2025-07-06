/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** CWDCommand.hpp
*/

#pragma once

#include <unistd.h>

#include "SocketActions.hpp"
#include "FTPCommand.hpp"

class CWDCommand : public FTPCommand {
public:
    std::string execute(const std::string& args, ClientSession& session) override;
};
