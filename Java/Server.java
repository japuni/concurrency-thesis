package Java;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;

public class Server {
    static final boolean runParallel = true;
    static final boolean runParallelConnections = true;
    static ServerSocket serverSocket;

    private final static int amountOfThreads = 10;
    public static void main(String[] args) throws Exception {
        if (runParallelConnections) {
            try {
                serverSocket = new ServerSocket(8000);
                for (int i = 0; i < amountOfThreads; i++) {
                    new Thread(new ConnectionHandler(i+1)).start();
                }
            } catch (IOException e) {
                System.out.println(e);
            }
        } else {
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
    }

    static void handleClientSeq(Socket clientSocket) {
        handleRequest(clientSocket);
    }

    static void handleRequest(Socket clientSocket) {
        try (PrintWriter out = new PrintWriter(new BufferedOutputStream(clientSocket.getOutputStream()))) {
            out.print("ok");
            out.flush();

            BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
            String line = in.readLine();
            while (line != null) {
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

class ConnectionHandler implements Runnable {

    private int workerNr;

    public ConnectionHandler(int i) {
        this.workerNr = i;
    }
    @Override
    public void run() {
        System.out.println("Started connection thread");
        while (true) {
            try {
                Socket socket = Server.serverSocket.accept();
                System.out.println("Thread " + workerNr + " accepted connection: " + socket.getInetAddress() + ":" + socket.getPort());
                if (Server.runParallel) {
                    new Thread(new ClientHandler(socket)).start();
                } else {
                    Server.handleClientSeq(socket);
                }
                System.out.println("Thread " + workerNr + " has handled its client");
            } catch (IOException e) {
                System.out.println("I/O error: " + e);
            }
        }
    }
}
