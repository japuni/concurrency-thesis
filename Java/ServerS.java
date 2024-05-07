package Java;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;

public class ServerS {
    static ServerSocket serverSocket;

    static boolean runMatrix = true;

    public static void main(String[] args) {
        try {
            serverSocket = new ServerSocket(8000);
            while (true) {
                try {
                    Socket socket = serverSocket.accept();
                    try {
                        PrintWriter out = new PrintWriter(new BufferedOutputStream(socket.getOutputStream()));
                        BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                        try {
                            if (runMatrix) {
                                String response = handleRequestMatrix(in);
                            } else {
                                String response = handleRequestArithmetic(in);
                            }
                        } catch (IOException e) {
                            System.out.println("I/O error: " + e);
                        } finally {
                            try {
                                socket.close();
                            } catch (IOException e) {
                                System.out.println("I/O error: " + e);
                            }
                        }
                    } catch (IOException e) {
                        System.out.println("I/O error: " + e);
                    }
                } catch (IOException e) {
                    System.out.println("I/O error: " + e);
                }
            }
        } catch (IOException e) {
            System.out.println("I/O error: " + e);
        }
    }

    static String handleRequestArithmetic(BufferedReader in) throws IOException {
        String line = in.readLine();
        while (line != null) {
            line = in.readLine();
        }
        String[] requestParts = line.split("\\+");
        int result = Integer.parseInt(requestParts[0]) + Integer.parseInt(requestParts[1]);
        return Integer.toString(result);
    }

    static String handleRequestMatrix(BufferedReader in) throws IOException {
        String line = in.readLine();
        List<String> matrices = new ArrayList<>();
        while (line != null) {
            matrices.add(line);
            line = in.readLine();
        }
        return "hello";
    }

    static int[][] convertStringToMatrix(String matrix) {
        int[][] temp = new int[2][2];
        return temp;
    }
}


