package Java;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;

public class Server {
    static final boolean runParallel = true;
    static ServerSocket serverSocket;

    private final static int amountOfThreads = 6;
    public static void main(String[] args) throws Exception {
        if (runParallel) {
            try {
                serverSocket = new ServerSocket(8000);
                for (int i = 0; i < amountOfThreads; i++) {
                    new Thread(new ConnectionHandler(i + 1)).start();
                }
            } catch (IOException e) {
                System.out.println("I/O error: " + e);
            }
        } else {
            try (ServerSocket serverSocket = new ServerSocket(8000)) {
                while (true) {
                    try {
                        Socket socket = serverSocket.accept();
                        handleRequest(socket);
                    } catch (IOException e) {
                        System.out.println("I/O error: " + e);
                    }
                }
            }
        }
    }

    static void handleRequest(Socket clientSocket) {
        try (PrintWriter out = new PrintWriter(new BufferedOutputStream(clientSocket.getOutputStream()))) {
            BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
            String line = in.readLine();
            while (line != null) {
                out.print("ok");
                out.flush();
                System.out.println(line);
                line = in.readLine();
            }
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

    static class ClientHandler implements Runnable {
        private final Socket clientSocket;
        public ClientHandler(Socket clientSocket) {
            this.clientSocket = clientSocket;
        }
        @Override
        public void run() {
            Server.handleRequest(clientSocket);
        }
    }

    static class ConnectionHandler implements Runnable {

        private final int workerNr;

        public ConnectionHandler(int i) {
            this.workerNr = i;
        }

        @Override
        public void run() {
            System.out.println("Started connection thread");
            while (true) {
                try {
                    Socket socket = serverSocket.accept();
                    System.out.println("Thread " + workerNr + " accepted connection: " + socket.getInetAddress() + ":" + socket.getPort());

                    for (int i = 0; i < amountOfThreads; i++) {
                        new Thread(new ClientHandler(socket)).start();
                    }

                    System.out.println("Thread " + workerNr + " has handled its client");
                } catch (IOException e) {
                    System.out.println("I/O error: " + e);
                }
            }
        }
    }
}


