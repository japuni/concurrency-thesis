package Java;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Arrays;

public class ServerS {
    private static ServerSocket serverSocket;
    private static final boolean runMatrix = true;

    public static void main(String[] args) {
        try {
            serverSocket = new ServerSocket(8000);
            while (true) {
                Socket socket = serverSocket.accept();
                handleClientRequest(socket);
            }
        } catch (IOException e) {
            logError(e);
        }
    }

    private static void handleClientRequest(Socket socket) {
        try {
            PrintWriter out = new PrintWriter(new BufferedOutputStream(socket.getOutputStream()));
            BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));

            if (runMatrix) {
                processMatrixReq(in, out);
            } else {
                String response = handleRequestArithmetic(in);
                out.print(response);
                out.flush();
            }

            socket.close();
        } catch (IOException e) {
            logError(e);
        }
    }

    private static void processMatrixReq(BufferedReader in, PrintWriter out) throws IOException {
        String message = in.readLine();
        int[][] matrix = handleRequestMatrix(message);

        int[][] result = MatrixMultiplier.multiplyMatricesParallel(matrix, matrix);
        out.print(Arrays.deepToString(result));
        out.flush();
    }

    private static void logError(IOException e) {
        System.out.println("I/O error: " + e);
    }

    private static String handleRequestArithmetic(BufferedReader in) throws IOException {
        String line = in.readLine();
        while (line != null) {
            line = in.readLine();
        }
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