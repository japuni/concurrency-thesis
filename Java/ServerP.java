package Java;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Arrays;

public class ServerP {
    private static ServerSocket serverSocket;
    private static final int amountOfThreads = 6;
    private static final boolean runMatrix = true;

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

    static class ConnectionHandler implements Runnable {
        @Override
        public void run() {
            while (true) {
                try {
                    Socket socket = serverSocket.accept();
                    new Thread(new ClientHandler(socket)).start();
                } catch (IOException e) {
                    System.out.println("I/O error: " + e);
                }
            }
        }
    }

    static class ClientHandler implements Runnable {
        PrintWriter out;
        BufferedReader in;
        Socket clientSocket;

        ClientHandler(Socket clientSocket){
            this.clientSocket = clientSocket;
        }

        @Override
        public void run() {
            try {
                out = new PrintWriter(new BufferedOutputStream(clientSocket.getOutputStream()));
                in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));

                if (runMatrix) {
                    processMatrixReq(in, out);
                } else {
                    String response = handleRequestArithmetic(in.readLine());
                    out.print(response);
                    out.flush();
                }

                clientSocket.close();
            } catch (IOException e) {
                System.out.println("I/O error: " + e);
            }
        }
    }

    private static void processMatrixReq(BufferedReader in, PrintWriter out) throws IOException {
        String message = in.readLine();
        int[][] matrix = handleRequestMatrix(message);

        int[][] result = MatrixMultiplier.multiplyMatricesParallel(matrix, matrix);
        out.print(Arrays.deepToString(result));
        out.flush();
    }

    private static String handleRequestArithmetic(String request) {
        String[] requestParts = request.split("\\+");
        int result = Integer.parseInt(requestParts[0]) + Integer.parseInt(requestParts[1]);
        return Integer.toString(result);
    }

    private static int[][] handleRequestMatrix(String matrixDescription) {
        matrixDescription = matrixDescription.substring(1, matrixDescription.length() - 1);
        String[] rows = matrixDescription.split("\\],\\[");
        int[][] newMatrix = new int[rows.length][];

        for (int i = 0; i < rows.length; i++) {
            String row = rows[i];
            row = row.replaceAll("\\[|\\]", "");
            String[] numbers = row.split(",");
            newMatrix[i] = new int[numbers.length];
            for (int j = 0; j < numbers.length; j++) {
                newMatrix[i][j] = Integer.parseInt(numbers[j]);
            }
        }

        return newMatrix;
    }
}