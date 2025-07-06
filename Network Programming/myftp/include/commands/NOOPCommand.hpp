/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** NOOPCommand.hpp
*/

#pragma once

#include "FTPCommand.hpp"

class NOOPCommand : public FTPCommand {
public:
    std::string execute(const std::string& args, ClientSession& session) override;
};
