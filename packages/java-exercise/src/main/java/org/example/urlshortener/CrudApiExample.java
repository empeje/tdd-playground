package org.example.urlshortener;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;

public class CrudApiExample {

    private static Connection connection;

    public static void main(String[] args) throws Exception {
        startDatabase();
        HttpServer server = HttpServer.create(new InetSocketAddress(8080), 0);
        server.createContext("/users", new UserHandler());
        server.setExecutor(null);
        server.start();
        System.out.println("Server running on http://localhost:8080/users");
    }

    public static synchronized void startDatabase() throws SQLException {
        if (connection == null || connection.isClosed()) {
            connection = DriverManager.getConnection("jdbc:h2:mem:testdb;DB_CLOSE_DELAY=-1", "sa", "");
            String createTableSQL = "CREATE TABLE IF NOT EXISTS users (id INT AUTO_INCREMENT PRIMARY KEY, name VARCHAR(255))";
            try (Statement stmt = connection.createStatement()) {
                stmt.execute(createTableSQL);
            }
        }
    }

    public static synchronized Connection getConnection() throws SQLException {
        if (connection == null || connection.isClosed()) {
            startDatabase();
        }
        return connection;
    }

    public static class UserHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            String method = exchange.getRequestMethod();
            String query = exchange.getRequestURI().getQuery();

            try {
                if ("GET".equalsIgnoreCase(method)) {
                    String response = getAllUsers();
                    sendResponse(exchange, 200, response);
                } else if ("POST".equalsIgnoreCase(method)) {
                    String name = new String(exchange.getRequestBody().readAllBytes()).trim();
                    if (name.isEmpty()) {
                        sendResponse(exchange, 400, "Name is required");
                        return;
                    }
                    int newId = createUser(name);
                    sendResponse(exchange, 201, "User created with id " + newId);
                } else if ("PUT".equalsIgnoreCase(method)) {
                    Map<String, String> params = queryToMap(query);
                    if (!params.containsKey("id")) {
                        sendResponse(exchange, 400, "Missing id parameter");
                        return;
                    }
                    int id = Integer.parseInt(params.get("id"));
                    String newName = new String(exchange.getRequestBody().readAllBytes()).trim();
                    if (updateUser(id, newName)) {
                        sendResponse(exchange, 200, "User updated");
                    } else {
                        sendResponse(exchange, 404, "User not found");
                    }
                } else if ("DELETE".equalsIgnoreCase(method)) {
                    Map<String, String> params = queryToMap(query);
                    if (!params.containsKey("id")) {
                        sendResponse(exchange, 400, "Missing id parameter");
                        return;
                    }
                    int id = Integer.parseInt(params.get("id"));
                    if (deleteUser(id)) {
                        sendResponse(exchange, 200, "User deleted");
                    } else {
                        sendResponse(exchange, 404, "User not found");
                    }
                } else {
                    sendResponse(exchange, 405, "Method Not Allowed");
                }
            } catch (Exception e) {
                sendResponse(exchange, 500, "Internal server error: " + e.getMessage());
            }
        }
    }

    public static void sendResponse(HttpExchange exchange, int statusCode, String response) throws IOException {
        byte[] bytes = response.getBytes();
        exchange.getResponseHeaders().add("Content-Type", "text/plain; charset=utf-8");
        exchange.sendResponseHeaders(statusCode, bytes.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(bytes);
        }
    }

    public static String getAllUsers() throws SQLException {
        StringBuilder sb = new StringBuilder();
        try (Statement stmt = getConnection().createStatement();
             ResultSet rs = stmt.executeQuery("SELECT * FROM users")) {
            while (rs.next()) {
                sb.append("ID: ").append(rs.getInt("id"))
                        .append(", Name: ").append(rs.getString("name"))
                        .append("\n");
            }
        }
        return sb.length() == 0 ? "No users found" : sb.toString();
    }

    public static int createUser(String name) throws SQLException {
        String sql = "INSERT INTO users (name) VALUES (?)";
        try (PreparedStatement ps = getConnection().prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)) {
            ps.setString(1, name);
            ps.executeUpdate();
            try (ResultSet keys = ps.getGeneratedKeys()) {
                if (keys.next()) {
                    return keys.getInt(1);
                }
            }
        }
        throw new SQLException("User ID retrieval failed");
    }

    public static boolean updateUser(int id, String newName) throws SQLException {
        String sql = "UPDATE users SET name = ? WHERE id = ?";
        try (PreparedStatement ps = getConnection().prepareStatement(sql)) {
            ps.setString(1, newName);
            ps.setInt(2, id);
            int affected = ps.executeUpdate();
            return affected > 0;
        }
    }

    public static boolean deleteUser(int id) throws SQLException {
        String sql = "DELETE FROM users WHERE id = ?";
        try (PreparedStatement ps = getConnection().prepareStatement(sql)) {
            ps.setInt(1, id);
            int affected = ps.executeUpdate();
            return affected > 0;
        }
    }

    public static Map<String, String> queryToMap(String query) {
        Map<String, String> map = new HashMap<>();
        if (query == null || query.isEmpty()) return map;
        for (String param : query.split("&")) {
            String[] pair = param.split("=");
            if (pair.length == 2) {
                map.put(pair[0], pair[1]);
            }
        }
        return map;
    }
}
