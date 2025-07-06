/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** HELPCommand.hpp
*/

#pragma once

#include "FTPCommand.hpp"

class HELPCommand : public FTPCommand {
public:
    std::string execute(const std::string& args, ClientSession& session) override;
};
