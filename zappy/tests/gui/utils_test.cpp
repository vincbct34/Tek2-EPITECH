/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Utils tests (exceptions and socket handling)
*/

#include <criterion/criterion.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

// Include the utility headers
#include "../src/utils/Exceptions.hpp"
#include "../src/utils/CustomSocketHandling.hpp"

// =============================================================================
// Exception Tests
// =============================================================================

Test(exceptions, base_exception_construction) {
    Exception ex("Test message");
    cr_assert_str_eq(ex.what(), "Test message");
}

Test(exceptions, args_exception_construction) {
    ArgsException ex("Invalid argument");
    // Should prefix with "Args Error: "
    cr_assert_str_eq(ex.what(), "Args Error: Invalid argument");
}

Test(exceptions, network_exception_construction) {
    NetworkException ex("Connection failed");
    // Should prefix with "Network Error: "
    cr_assert_str_eq(ex.what(), "Network Error: Connection failed");
}

Test(exceptions, exception_inheritance) {
    // Test that our exceptions can be caught as std::runtime_error
    try {
        throw ArgsException("test");
    } catch (const std::runtime_error& e) {
        cr_assert_str_eq(e.what(), "Args Error: test");
    }
    
    try {
        throw NetworkException("network test");
    } catch (const Exception& e) {
        cr_assert_str_eq(e.what(), "Network Error: network test");
    }
}

Test(exceptions, empty_message) {
    Exception ex("");
    cr_assert_str_eq(ex.what(), "");
    
    ArgsException args_ex("");
    cr_assert_str_eq(args_ex.what(), "Args Error: ");
    
    NetworkException net_ex("");
    cr_assert_str_eq(net_ex.what(), "Network Error: ");
}

// =============================================================================
// CustomSocketHandling Tests
// =============================================================================

Test(socket_handling, open_socket_valid) {
    int sockfd = CustomSocketHandling::openSocket(AF_INET, SOCK_STREAM, 0);
    cr_assert_neq(sockfd, -1, "Should be able to open a valid socket");
    
    if (sockfd != -1) {
        CustomSocketHandling::closeSocket(sockfd);
    }
}

Test(socket_handling, open_socket_invalid_domain) {
    // Test with invalid domain
    int sockfd = CustomSocketHandling::openSocket(-1, SOCK_STREAM, 0);
    cr_assert_eq(sockfd, -1, "Should fail with invalid domain");
}

Test(socket_handling, close_socket) {
    int sockfd = CustomSocketHandling::openSocket(AF_INET, SOCK_STREAM, 0);
    cr_assert_neq(sockfd, -1);
    
    // This should not crash
    CustomSocketHandling::closeSocket(sockfd);
    cr_assert(true, "closeSocket should complete without crashing");
}

Test(socket_handling, custom_htons) {
    uint16_t host_port = 8080;
    uint16_t net_port = CustomSocketHandling::customHtons(host_port);
    
    // Should be equal to standard htons
    cr_assert_eq(net_port, htons(host_port));
}

Test(socket_handling, custom_htons_zero) {
    uint16_t result = CustomSocketHandling::customHtons(0);
    cr_assert_eq(result, 0);
}

Test(socket_handling, custom_htons_max_port) {
    uint16_t max_port = 65535;
    uint16_t result = CustomSocketHandling::customHtons(max_port);
    cr_assert_eq(result, htons(max_port));
}

Test(socket_handling, inet_pton_valid_ipv4) {
    struct in_addr addr;
    int result = CustomSocketHandling::customInetPton(AF_INET, "127.0.0.1", &addr);
    cr_assert_eq(result, 1, "Should successfully convert valid IPv4 address");
}

Test(socket_handling, inet_pton_invalid_ipv4) {
    struct in_addr addr;
    int result = CustomSocketHandling::customInetPton(AF_INET, "256.256.256.256", &addr);
    cr_assert_eq(result, 0, "Should fail with invalid IPv4 address");
}

Test(socket_handling, inet_pton_malformed_ip) {
    struct in_addr addr;
    int result = CustomSocketHandling::customInetPton(AF_INET, "not.an.ip.address", &addr);
    cr_assert_eq(result, 0, "Should fail with malformed IP");
}

Test(socket_handling, inet_pton_empty_string) {
    struct in_addr addr;
    int result = CustomSocketHandling::customInetPton(AF_INET, "", &addr);
    cr_assert_eq(result, 0, "Should fail with empty string");
}

Test(socket_handling, connect_to_server_invalid_socket) {
    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(8080);
    server_addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    
    // Use an invalid socket descriptor
    int result = CustomSocketHandling::connectToServer(-1, 
        (struct sockaddr*)&server_addr, sizeof(server_addr));
    cr_assert_eq(result, -1, "Should fail with invalid socket");
}

Test(socket_handling, write_to_invalid_socket) {
    const char* data = "test data";
    ssize_t result = CustomSocketHandling::writeToSocket(-1, data, strlen(data));
    cr_assert_eq(result, -1, "Should fail writing to invalid socket");
}

// =============================================================================
// Integration Tests
// =============================================================================

Test(integration, socket_lifecycle) {
    // Test a complete socket lifecycle
    int sockfd = CustomSocketHandling::openSocket(AF_INET, SOCK_STREAM, 0);
    cr_assert_neq(sockfd, -1, "Should create socket");
    
    // Try to prepare connection (won't actually connect, just test structure)
    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = CustomSocketHandling::customHtons(8080);
    
    int inet_result = CustomSocketHandling::customInetPton(AF_INET, "127.0.0.1", &server_addr.sin_addr);
    cr_assert_eq(inet_result, 1, "Should convert IP address");
    
    // Don't actually connect, just test that the address was set up correctly
    cr_assert_eq(server_addr.sin_family, AF_INET);
    cr_assert_eq(server_addr.sin_port, htons(8080));
    
    CustomSocketHandling::closeSocket(sockfd);
}

Test(integration, exception_throwing_and_catching) {
    // Test that we can throw and catch different exception types
    bool caught_args = false;
    bool caught_network = false;
    bool caught_base = false;
    
    try {
        throw ArgsException("args test");
    } catch (const ArgsException& e) {
        caught_args = true;
        cr_assert_str_eq(e.what(), "Args Error: args test");
    }
    
    try {
        throw NetworkException("network test");
    } catch (const NetworkException& e) {
        caught_network = true;
        cr_assert_str_eq(e.what(), "Network Error: network test");
    }
    
    try {
        throw Exception("base test");
    } catch (const Exception& e) {
        caught_base = true;
        cr_assert_str_eq(e.what(), "base test");
    }
    
    cr_assert(caught_args, "Should have caught ArgsException");
    cr_assert(caught_network, "Should have caught NetworkException");
    cr_assert(caught_base, "Should have caught base Exception");
}

// =============================================================================
// Edge Case Tests
// =============================================================================

Test(edge_cases, multiple_socket_operations) {
    // Test creating and closing multiple sockets
    int sockets[5];
    
    for (int i = 0; i < 5; i++) {
        sockets[i] = CustomSocketHandling::openSocket(AF_INET, SOCK_STREAM, 0);
        cr_assert_neq(sockets[i], -1, "Should create socket %d", i);
    }
    
    for (int i = 0; i < 5; i++) {
        CustomSocketHandling::closeSocket(sockets[i]);
    }
}

Test(edge_cases, exception_message_boundaries) {
    // Test with very long messages
    std::string long_message(1000, 'x');
    Exception long_ex(long_message);
    cr_assert_str_eq(long_ex.what(), long_message.c_str());
    
    ArgsException long_args(long_message);
    std::string expected_args = "Args Error: " + long_message;
    cr_assert_str_eq(long_args.what(), expected_args.c_str());
}

Test(edge_cases, htons_endianness) {
    // Test various port values to ensure endianness handling
    uint16_t ports[] = {1, 80, 443, 8080, 65535};
    
    for (size_t i = 0; i < sizeof(ports)/sizeof(ports[0]); i++) {
        uint16_t converted = CustomSocketHandling::customHtons(ports[i]);
        cr_assert_eq(converted, htons(ports[i]), "Port %d conversion mismatch", ports[i]);
    }
}
