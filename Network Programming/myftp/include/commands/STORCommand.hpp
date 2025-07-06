/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** STORCommand.hpp
*/

#pragma once

#include <sys/wait.h>
#include <unistd.h>
#include <fstream>

#include "ClientSession.hpp"
#include "SocketActions.hpp"
#include "FTPCommand.hpp"

class STORCommand : public FTPCommand {
public:
    std::string execute(const std::string& args, ClientSession& session) override;
};
