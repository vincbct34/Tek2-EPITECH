/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** QuitCommand.hpp
*/

#pragma once

#include <unistd.h>

#include "ClientSession.hpp"
#include "FTPCommand.hpp"

class QUITCommand : public FTPCommand {
public:
    std::string execute(const std::string& args, ClientSession& session) override;
};
