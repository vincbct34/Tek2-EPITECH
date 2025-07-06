/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** Exceptions.hpp
*/

#pragma once

#include <exception>
#include <string>

class FileError : public std::exception {
public:
    explicit FileError(const std::string &message);
    const char* what() const noexcept override;

private:
    std::string _message;
};

class ComponentTypeError : public std::exception {
public:
    explicit ComponentTypeError(const std::string &type);
    const char* what() const noexcept override;

private:
    std::string _message;
};

class ComponentNameError : public std::exception {
public:
    explicit ComponentNameError(const std::string &name);
    const char* what() const noexcept override;

private:
    std::string _message;
};

class DuplicateComponentError : public std::exception {
public:
    explicit DuplicateComponentError(const std::string &name);
    const char* what() const noexcept override;

private:
    std::string _message;
};

class NoChipsetsError : public std::exception {
public:
    NoChipsetsError();
    const char* what() const noexcept override;

private:
    std::string _message;
};

class BadArgumentsError : public std::exception {
public:
    BadArgumentsError();
    const char* what() const noexcept override;

private:
    std::string _message;
};

class NoLinksError : public std::exception {
public:
    NoLinksError();
    const char* what() const noexcept override;

private:
    std::string _message;
};

class SyntaxicError : public std::exception {
public:
    SyntaxicError(const std::string &message);
    const char* what() const noexcept override;

private:
    std::string _message;
};
