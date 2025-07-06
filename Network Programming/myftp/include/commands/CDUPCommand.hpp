/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** CDUPCommand.hpp
*/

#pragma once

#include <unistd.h>

#include "SocketActions.hpp"
#include "FTPCommand.hpp"

class CDUPCommand : public FTPCommand {
public:
    std::string execute(const std::string& args, ClientSession& session) override;
};
