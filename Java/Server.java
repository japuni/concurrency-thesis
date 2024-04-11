package Java;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;

public class Server {
    static ServerSocket serverSocket;

    private final static int amountOfThreads = 6;
    public static void main(String[] args) {
        try {
            serverSocket = new ServerSocket(8000);
            for (int i = 0; i < amountOfThreads; i++) {
                new Thread(new ConnectionHandler()).start();
            }
        } catch (IOException e) {
            System.out.println("I/O error: " + e);
        }
    }

    static void handleRequest(String request, PrintWriter out) {
        String[] requestParts = request.split("\\+");
        int result = Integer.parseInt(requestParts[0]) + Integer.parseInt(requestParts[1]);
        out.print(result);
        out.flush();
    }

    static class ClientHandler implements Runnable {

        PrintWriter out;
        BufferedReader in;
        Socket clientSocket;

        ClientHandler(PrintWriter out, BufferedReader in, Socket clientSocket) {
            this.out = out;
            this.in = in;
            this.clientSocket = clientSocket;
        }

        @Override
        public void run() {
            try {

                String line = in.readLine();
                while (line != null) {
                    handleRequest(line, out);
                    line = in.readLine();
                }
            } catch (IOException e) {
                System.out.println("I/O error: " + e);
            } finally {
                try {
                    clientSocket.close();
                } catch (IOException e) {
                    System.out.println("I/O error: " + e);
                }
            }
        }
    }

    static class ConnectionHandler implements Runnable {

        @Override
        public void run() {
            while (true) {
                try {
                    Socket socket = serverSocket.accept();
                    try {
                        PrintWriter out = new PrintWriter(new BufferedOutputStream(socket.getOutputStream()));
                        BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));

                        for (int i = 0; i < amountOfThreads; i++) {
                            new Thread(new ClientHandler(out, in, socket)).start();
                        }
                    } catch (IOException e) {
                        System.out.println("I/O error: " + e);
                    }
                } catch (IOException e) {
                    System.out.println("I/O error: " + e);
                }
            }
        }
    }
}


