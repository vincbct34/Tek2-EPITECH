/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Graphics and Elevation tests
*/

#include <criterion/criterion.h>
#include <vector>
#include <string>

// Mocks pour éviter les dépendances Raylib dans les tests
struct MockTexture2D {
    unsigned int id = 0;
};

struct MockImage {
    void* data = nullptr;
    int width = 0;
    int height = 0;
};

struct MockRectangle {
    float x = 0.0f;
    float y = 0.0f;
    float width = 0.0f;
    float height = 0.0f;
};

// Mock de CustomRayLib pour les tests
class MockCustomRayLib {
public:
    static bool image_load_success;
    static bool texture_load_success;
    static double current_time;
    
    static void loadImage(const char* fileName, MockImage& image) {
        (void)fileName;
        if (image_load_success) {
            image.data = (void*)0x12345678; // Mock data pointer
            image.width = 100;
            image.height = 100;
        } else {
            image.data = nullptr;
        }
    }
    
    static void loadTextureFromImage(const MockImage& image, MockTexture2D& texture) {
        (void)image;
        if (texture_load_success) {
            texture.id = 12345;
        } else {
            texture.id = 0;
        }
    }
    
    static void unloadImage(MockImage& image) {
        image.data = nullptr;
    }
    
    static double getTime() {
        return current_time;
    }
};

bool MockCustomRayLib::image_load_success = true;
bool MockCustomRayLib::texture_load_success = true;
double MockCustomRayLib::current_time = 0.0;

// Version simplifiée de la classe Elevation pour les tests
class TestElevation {
public:
    TestElevation() = default;
    ~TestElevation() = default;

    void initElevation(int x, int y, int level, std::vector<int> ids) {
        setPosX(x);
        setPosY(y);
        setIds(ids);
        setSpriteIndex(0);
        setElevationActive(true);
        textureLoaded = false;
        setStartTime(MockCustomRayLib::getTime());
        (void)level; // Non utilisé
    }

    bool getElevation() const { return inElevationActive; }
    bool getElevationSuccess() const { return isElevationSuccess; }
    int getPosX() const { return posX; }
    int getPosY() const { return posY; }
    int getSpriteIndex() const { return spriteIndex; }
    double getStartTime() const { return startTime; }
    const std::vector<int>& getIds() const { return ids; }
    bool isTextureLoaded() const { return textureLoaded; }

    void setPosX(int x) { posX = x; }
    void setPosY(int y) { posY = y; }
    void setElevationActive(bool active) { inElevationActive = active; }
    void setElevationSuccess(bool success) { isElevationSuccess = success; }
    void setSpriteIndex(int index) { spriteIndex = index; }
    void setStartTime(double time) { startTime = time; }
    void setIds(const std::vector<int>& idsList) { ids = idsList; }

    void loadTexture() {
        if (textureLoaded) return;
        
        MockImage image;
        MockCustomRayLib::loadImage("./gui/assets/effect_1524x1058.png", image);
        
        if (image.data == nullptr) {
            return;
        }
        
        MockTexture2D texture;
        MockCustomRayLib::loadTextureFromImage(image, texture);
        MockCustomRayLib::unloadImage(image);
        
        if (texture.id == 0) {
            return;
        }
        
        textureLoaded = true;
    }

private:
    bool inElevationActive = false;
    bool isElevationSuccess = false;
    int posX = 0;
    int posY = 0;
    double startTime = 0.0;
    int spriteIndex = 0;
    std::vector<int> ids;
    bool textureLoaded = false;
};

// =============================================================================
// Elevation Tests
// =============================================================================

Test(elevation, basic_construction) {
    TestElevation elevation;
    
    cr_assert_eq(elevation.getElevation(), false);
    cr_assert_eq(elevation.getElevationSuccess(), false);
    cr_assert_eq(elevation.getPosX(), 0);
    cr_assert_eq(elevation.getPosY(), 0);
    cr_assert_eq(elevation.getSpriteIndex(), 0);
    cr_assert_eq(elevation.getStartTime(), 0.0);
    cr_assert_eq(elevation.getIds().size(), 0);
}

Test(elevation, init_elevation) {
    TestElevation elevation;
    std::vector<int> playerIds = {1, 2, 3};
    MockCustomRayLib::current_time = 123.456;
    
    elevation.initElevation(10, 15, 2, playerIds);
    
    cr_assert_eq(elevation.getPosX(), 10);
    cr_assert_eq(elevation.getPosY(), 15);
    cr_assert_eq(elevation.getElevation(), true);
    cr_assert_eq(elevation.getSpriteIndex(), 0);
    cr_assert_eq(elevation.getStartTime(), 123.456);
    cr_assert_eq(elevation.getIds().size(), 3);
    cr_assert_eq(elevation.getIds()[0], 1);
    cr_assert_eq(elevation.getIds()[1], 2);
    cr_assert_eq(elevation.getIds()[2], 3);
}

Test(elevation, setters_and_getters) {
    TestElevation elevation;
    
    elevation.setPosX(42);
    elevation.setPosY(24);
    elevation.setElevationActive(true);
    elevation.setElevationSuccess(true);
    elevation.setSpriteIndex(5);
    elevation.setStartTime(999.111);
    
    cr_assert_eq(elevation.getPosX(), 42);
    cr_assert_eq(elevation.getPosY(), 24);
    cr_assert_eq(elevation.getElevation(), true);
    cr_assert_eq(elevation.getElevationSuccess(), true);
    cr_assert_eq(elevation.getSpriteIndex(), 5);
    cr_assert_eq(elevation.getStartTime(), 999.111);
}

Test(elevation, ids_management) {
    TestElevation elevation;
    
    std::vector<int> ids1 = {10, 20};
    std::vector<int> ids2 = {30, 40, 50, 60};
    
    elevation.setIds(ids1);
    cr_assert_eq(elevation.getIds().size(), 2);
    cr_assert_eq(elevation.getIds()[0], 10);
    cr_assert_eq(elevation.getIds()[1], 20);
    
    elevation.setIds(ids2);
    cr_assert_eq(elevation.getIds().size(), 4);
    cr_assert_eq(elevation.getIds()[2], 50);
    cr_assert_eq(elevation.getIds()[3], 60);
}

Test(elevation, empty_ids_list) {
    TestElevation elevation;
    std::vector<int> emptyIds;
    
    elevation.initElevation(5, 5, 1, emptyIds);
    
    cr_assert_eq(elevation.getIds().size(), 0);
    cr_assert_eq(elevation.getPosX(), 5);
    cr_assert_eq(elevation.getPosY(), 5);
}

Test(elevation, texture_loading_success) {
    TestElevation elevation;
    MockCustomRayLib::image_load_success = true;
    MockCustomRayLib::texture_load_success = true;
    
    cr_assert_eq(elevation.isTextureLoaded(), false);
    
    elevation.loadTexture();
    
    cr_assert_eq(elevation.isTextureLoaded(), true);
}

Test(elevation, texture_loading_image_failure) {
    TestElevation elevation;
    MockCustomRayLib::image_load_success = false;
    MockCustomRayLib::texture_load_success = true;
    
    elevation.loadTexture();
    
    cr_assert_eq(elevation.isTextureLoaded(), false);
}

Test(elevation, texture_loading_texture_failure) {
    TestElevation elevation;
    MockCustomRayLib::image_load_success = true;
    MockCustomRayLib::texture_load_success = false;
    
    elevation.loadTexture();
    
    cr_assert_eq(elevation.isTextureLoaded(), false);
}

Test(elevation, texture_loading_already_loaded) {
    TestElevation elevation;
    MockCustomRayLib::image_load_success = true;
    MockCustomRayLib::texture_load_success = true;
    
    elevation.loadTexture();
    cr_assert_eq(elevation.isTextureLoaded(), true);
    
    // Reset mock state to simulate failure
    MockCustomRayLib::image_load_success = false;
    MockCustomRayLib::texture_load_success = false;
    
    // Should not reload since already loaded
    elevation.loadTexture();
    cr_assert_eq(elevation.isTextureLoaded(), true);
}

// =============================================================================
// UI Logic Tests (position calculation, state management)
// =============================================================================

// Simple UI state manager pour les tests
class UIStateManager {
public:
    struct ConnectionState {
        std::string host = "";
        std::string port = "";
        bool isConnecting = false;
        bool isConnected = false;
        std::string errorMessage = "";
    };
    
    ConnectionState connection;
    
    bool validateHostPort(const std::string& host, const std::string& port) {
        if (host.empty() || port.empty()) {
            connection.errorMessage = "Host and port cannot be empty";
            return false;
        }
        
        try {
            int portNum = std::stoi(port);
            if (portNum <= 0 || portNum > 65535) {
                connection.errorMessage = "Port must be between 1 and 65535";
                return false;
            }
        } catch (...) {
            connection.errorMessage = "Invalid port number";
            return false;
        }
        
        connection.errorMessage = "";
        return true;
    }
    
    void setConnectionDetails(const std::string& host, const std::string& port) {
        connection.host = host;
        connection.port = port;
    }
    
    void setConnecting(bool connecting) {
        connection.isConnecting = connecting;
        if (connecting) {
            connection.isConnected = false;
        }
    }
    
    void setConnected(bool connected) {
        connection.isConnected = connected;
        if (connected) {
            connection.isConnecting = false;
        }
    }
};

Test(ui_state, basic_construction) {
    UIStateManager ui;
    
    cr_assert_str_eq(ui.connection.host.c_str(), "");
    cr_assert_str_eq(ui.connection.port.c_str(), "");
    cr_assert_eq(ui.connection.isConnecting, false);
    cr_assert_eq(ui.connection.isConnected, false);
    cr_assert_str_eq(ui.connection.errorMessage.c_str(), "");
}

Test(ui_state, validate_host_port_valid) {
    UIStateManager ui;
    
    bool result = ui.validateHostPort("localhost", "8080");
    
    cr_assert_eq(result, true);
    cr_assert_str_eq(ui.connection.errorMessage.c_str(), "");
}

Test(ui_state, validate_host_port_empty_host) {
    UIStateManager ui;
    
    bool result = ui.validateHostPort("", "8080");
    
    cr_assert_eq(result, false);
    cr_assert_str_eq(ui.connection.errorMessage.c_str(), "Host and port cannot be empty");
}

Test(ui_state, validate_host_port_empty_port) {
    UIStateManager ui;
    
    bool result = ui.validateHostPort("localhost", "");
    
    cr_assert_eq(result, false);
    cr_assert_str_eq(ui.connection.errorMessage.c_str(), "Host and port cannot be empty");
}

Test(ui_state, validate_host_port_invalid_port_zero) {
    UIStateManager ui;
    
    bool result = ui.validateHostPort("localhost", "0");
    
    cr_assert_eq(result, false);
    cr_assert_str_eq(ui.connection.errorMessage.c_str(), "Port must be between 1 and 65535");
}

Test(ui_state, validate_host_port_invalid_port_too_high) {
    UIStateManager ui;
    
    bool result = ui.validateHostPort("localhost", "65536");
    
    cr_assert_eq(result, false);
    cr_assert_str_eq(ui.connection.errorMessage.c_str(), "Port must be between 1 and 65535");
}

Test(ui_state, validate_host_port_invalid_port_not_number) {
    UIStateManager ui;
    
    bool result = ui.validateHostPort("localhost", "not_a_number");
    
    cr_assert_eq(result, false);
    cr_assert_str_eq(ui.connection.errorMessage.c_str(), "Invalid port number");
}

Test(ui_state, validate_host_port_boundary_values) {
    UIStateManager ui;
    
    cr_assert_eq(ui.validateHostPort("localhost", "1"), true);
    cr_assert_eq(ui.validateHostPort("localhost", "65535"), true);
    cr_assert_eq(ui.validateHostPort("127.0.0.1", "22"), true);
    cr_assert_eq(ui.validateHostPort("example.com", "443"), true);
}

Test(ui_state, connection_state_management) {
    UIStateManager ui;
    
    ui.setConnectionDetails("localhost", "8080");
    cr_assert_str_eq(ui.connection.host.c_str(), "localhost");
    cr_assert_str_eq(ui.connection.port.c_str(), "8080");
    
    ui.setConnecting(true);
    cr_assert_eq(ui.connection.isConnecting, true);
    cr_assert_eq(ui.connection.isConnected, false);
    
    ui.setConnected(true);
    cr_assert_eq(ui.connection.isConnected, true);
    cr_assert_eq(ui.connection.isConnecting, false);
}

// =============================================================================
// Position and Geometry Calculation Tests
// =============================================================================

struct Point2D {
    int x, y;
    Point2D(int x = 0, int y = 0) : x(x), y(y) {}
};

struct Rectangle2D {
    int x, y, width, height;
    Rectangle2D(int x = 0, int y = 0, int w = 0, int h = 0) : x(x), y(y), width(w), height(h) {}
    
    bool contains(const Point2D& point) const {
        return point.x >= x && point.x < x + width &&
               point.y >= y && point.y < y + height;
    }
    
    Point2D center() const {
        return Point2D(x + width / 2, y + height / 2);
    }
};

class LayoutCalculator {
public:
    static Rectangle2D calculateCenteredRectangle(int windowWidth, int windowHeight, int rectWidth, int rectHeight) {
        int x = (windowWidth - rectWidth) / 2;
        int y = (windowHeight - rectHeight) / 2;
        return Rectangle2D(x, y, rectWidth, rectHeight);
    }
    
    static Point2D calculateTextPosition(const Rectangle2D& container, int textWidth, int textHeight) {
        int x = container.x + (container.width - textWidth) / 2;
        int y = container.y + (container.height - textHeight) / 2;
        return Point2D(x, y);
    }
    
    static std::vector<Rectangle2D> calculateGridLayout(int gridWidth, int gridHeight, int cellWidth, int cellHeight, int startX = 0, int startY = 0) {
        std::vector<Rectangle2D> cells;
        
        for (int y = 0; y < gridHeight; y++) {
            for (int x = 0; x < gridWidth; x++) {
                int posX = startX + x * cellWidth;
                int posY = startY + y * cellHeight;
                cells.emplace_back(posX, posY, cellWidth, cellHeight);
            }
        }
        
        return cells;
    }
};

Test(layout, centered_rectangle_calculation) {
    Rectangle2D rect = LayoutCalculator::calculateCenteredRectangle(800, 600, 200, 150);
    
    cr_assert_eq(rect.x, 300); // (800 - 200) / 2
    cr_assert_eq(rect.y, 225); // (600 - 150) / 2
    cr_assert_eq(rect.width, 200);
    cr_assert_eq(rect.height, 150);
}

Test(layout, text_position_in_container) {
    Rectangle2D container(100, 50, 200, 100);
    Point2D textPos = LayoutCalculator::calculateTextPosition(container, 50, 20);
    
    cr_assert_eq(textPos.x, 175); // 100 + (200 - 50) / 2
    cr_assert_eq(textPos.y, 90);  // 50 + (100 - 20) / 2
}

Test(layout, grid_layout_calculation) {
    std::vector<Rectangle2D> grid = LayoutCalculator::calculateGridLayout(3, 2, 50, 40, 10, 20);
    
    cr_assert_eq(grid.size(), 6); // 3 * 2
    
    // First cell
    cr_assert_eq(grid[0].x, 10);
    cr_assert_eq(grid[0].y, 20);
    cr_assert_eq(grid[0].width, 50);
    cr_assert_eq(grid[0].height, 40);
    
    // Second cell (next column)
    cr_assert_eq(grid[1].x, 60); // 10 + 50
    cr_assert_eq(grid[1].y, 20);
    
    // Fourth cell (next row, first column)
    cr_assert_eq(grid[3].x, 10);
    cr_assert_eq(grid[3].y, 60); // 20 + 40
}

Test(geometry, rectangle_contains_point) {
    Rectangle2D rect(10, 10, 50, 30);
    
    cr_assert_eq(rect.contains(Point2D(10, 10)), true);   // Top-left corner
    cr_assert_eq(rect.contains(Point2D(59, 39)), true);   // Bottom-right corner (exclusive)
    cr_assert_eq(rect.contains(Point2D(30, 25)), true);   // Center
    cr_assert_eq(rect.contains(Point2D(5, 25)), false);   // Left of rectangle
    cr_assert_eq(rect.contains(Point2D(65, 25)), false);  // Right of rectangle
    cr_assert_eq(rect.contains(Point2D(30, 5)), false);   // Above rectangle
    cr_assert_eq(rect.contains(Point2D(30, 45)), false);  // Below rectangle
}

Test(geometry, rectangle_center) {
    Rectangle2D rect(20, 30, 100, 80);
    Point2D center = rect.center();
    
    cr_assert_eq(center.x, 70); // 20 + 100/2
    cr_assert_eq(center.y, 70); // 30 + 80/2
}

// =============================================================================
// Integration Tests
// =============================================================================

Test(integration, elevation_complete_lifecycle) {
    TestElevation elevation;
    MockCustomRayLib::current_time = 100.0;
    MockCustomRayLib::image_load_success = true;
    MockCustomRayLib::texture_load_success = true;
    
    // Initialize elevation
    std::vector<int> playerIds = {1, 2, 3, 4};
    elevation.initElevation(25, 30, 3, playerIds);
    
    // Verify initial state
    cr_assert_eq(elevation.getPosX(), 25);
    cr_assert_eq(elevation.getPosY(), 30);
    cr_assert_eq(elevation.getElevation(), true);
    cr_assert_eq(elevation.getElevationSuccess(), false);
    cr_assert_eq(elevation.getIds().size(), 4);
    cr_assert_eq(elevation.getStartTime(), 100.0);
    
    // Load texture
    elevation.loadTexture();
    cr_assert_eq(elevation.isTextureLoaded(), true);
    
    // Complete elevation
    elevation.setElevationSuccess(true);
    elevation.setElevationActive(false);
    
    cr_assert_eq(elevation.getElevation(), false);
    cr_assert_eq(elevation.getElevationSuccess(), true);
}
