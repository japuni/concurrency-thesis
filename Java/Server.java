package Java;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;

public class Server {
    private static final boolean runParallel = true;
    public static void main(String[] args) throws Exception {
        try (ServerSocket serverSocket = new ServerSocket(8000)) {
            while (true) {
                try {
                    Socket socket = serverSocket.accept();
                    if (runParallel) {
                        new Thread(new ClientHandler(socket)).start();
                    } else {
                        handleClientSeq(socket);
                    }
                } catch (IOException e) {
                    System.out.println("I/O error: " + e);
                }
            }
        }
    }

    static void handleClientSeq(Socket clientSocket) {
        handleRequest(clientSocket);
    }

    static void handleRequest(Socket clientSocket) {
        try (PrintWriter out = new PrintWriter(new BufferedOutputStream(clientSocket.getOutputStream()))) {
            out.print("ok");
            out.flush();
        } catch (IOException e) {
            System.out.println("Failed to send response to client: " + e.getMessage());
        } finally {
            try {
                clientSocket.close();
            } catch (IOException e) {
                System.out.println("Failed to close client socket: " + e.getMessage());
            }
        }
    }
}

class ClientHandler implements Runnable {
    private final Socket clientSocket;
    public ClientHandler(Socket clientSocket) {
        this.clientSocket = clientSocket;
    }
    @Override
    public void run() {
        Server.handleRequest(clientSocket);
    }
}
