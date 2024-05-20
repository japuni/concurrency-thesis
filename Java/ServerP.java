package Java;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Arrays;
import java.util.concurrent.ForkJoinPool;

public class ServerP {
    private static ServerSocket serverSocket;
    private static final int amountOfThreads = 24;
    private static final boolean runMatrix = false;

    public static void main(String[] args) {
        try {
            serverSocket = new ServerSocket(8000,100);
            new Thread(new ConnectionHandler()).start();
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
                    ForkJoinPool customThreadPool = new ForkJoinPool(amountOfThreads);
                    customThreadPool.submit(() ->
                            handleClient(socket)
                    );
                } catch (IOException e) {
                    System.out.println("I/O error: " + e);
                }
            }
        }
    }

    private static void handleClient(Socket clientSocket) {
        try {
            PrintWriter out = new PrintWriter(new BufferedOutputStream(clientSocket.getOutputStream()));
            BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));

            if (runMatrix) {
                processMatrixReq(in, out);
            } else {
                String line = in.readLine();
                while (line != null) {
                    String response = handleRequestArithmetic(in, line);
                    out.print(response);
                    out.flush();
                    line = in.readLine();
                }
            }

            clientSocket.close();
        } catch (IOException e) {
            System.out.println("I/O error: " + e);
        }
    }

    private static void processMatrixReq(BufferedReader in, PrintWriter out) throws IOException {
        StringBuilder matrixDescription = new StringBuilder();
        String message = in.readLine();
        while (!message.equals("EOF")) {
            if (message.contains("EOF")) {
                matrixDescription.append(message.replaceAll("EOF", ""));
                break;
            }
            matrixDescription.append(message);
            message = in.readLine();
        }
        int[][] matrix = handleRequestMatrix(matrixDescription.toString());

        MatrixMultiplier matrixMultiplier = new MatrixMultiplier();
        int[][] result = matrixMultiplier.multiplyMatricesParallel(matrix, matrix);
        out.print(Arrays.deepToString(result));
        out.flush();
    }

    private static String handleRequestArithmetic(BufferedReader in, String line) throws IOException {
        String[] requestParts = line.split("\\+");
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