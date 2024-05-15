package Java;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadMatrix {
    public static int[][] run(String matrixSize) throws IOException {
        BufferedReader bufferedReader = new BufferedReader(new FileReader("matrices/" + matrixSize + ".txt"));

        StringBuilder matrixDescription = new StringBuilder();
        String line;
        while ((line = bufferedReader.readLine()) != null) {
            matrixDescription.append(line);
        }
        bufferedReader.close();

        return createMatrix(matrixDescription.toString());
    }

    private static int[][] createMatrix(String matrixDescription) {
        matrixDescription = matrixDescription.substring(1, matrixDescription.length() - 1);
        String[] rows = matrixDescription.split("\\]\\[");
        int[][] newMatrix = new int[rows.length][];

        for (int i = 0; i < rows.length; i++) {
            String row = rows[i];
            row = row.replaceAll("\\[|\\]", "");
            String[] numbers = row.split(",");
            newMatrix[i] = new int[numbers.length];
            for (int j = 0; j < numbers.length; j++) {
                newMatrix[i][j] = Integer.parseInt(numbers[j].trim());
            }
        }

        return newMatrix;
    }
}
