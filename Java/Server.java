package Java;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;

public class Server {
    static ServerSocket serverSocket;

    private final static int amountOfThreads = 6;
    public static void main(String[] args) throws Exception {
        try {
            serverSocket = new ServerSocket(8000);
            for (int i = 0; i < amountOfThreads; i++) {
                new Thread(new ConnectionHandler(i + 1)).start();
            }
        } catch (IOException e) {
            System.out.println("I/O error: " + e);
        }
    }

    static void handleRequest(String request, PrintWriter out) {
        String[] requestParts = request.split("\\+");
        int result = Integer.parseInt(requestParts[0]) + Integer.parseInt(requestParts[1]);
        System.out.println(result);
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
                throw new RuntimeException(e);
            } finally {
                try {
                    clientSocket.close();
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
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
                    try {
                        System.out.println("Thread " + workerNr + " accepted connection: " + socket.getInetAddress() + ":" + socket.getPort());
                        PrintWriter out = new PrintWriter(new BufferedOutputStream(socket.getOutputStream()));
                        BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));

                        for (int i = 0; i < amountOfThreads; i++) {
                            new Thread(new ClientHandler(out, in, socket)).start();
                        }
                    } catch (IOException e) {
                        System.out.println("I/O error: " + e);
                    }
                    System.out.println("Thread " + workerNr + " has handled its client");
                } catch (IOException e) {
                    System.out.println("I/O error: " + e);
                }
            }
        }
    }
}


