package Java;

import java.io.FileWriter;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.ForkJoinPool;
import java.util.stream.IntStream;

public class MatrixMultiplier {
    private static int size;
    private static int amountOfTests;
    private static int threads;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);
        System.out.print("THREADS, AMOUNT OF TESTS, SIZE -> ");
        String input = scanner.nextLine();
        String[] splitInput = input.split(", ");
        threads = Integer.parseInt(splitInput[0]);
        amountOfTests = Integer.parseInt(splitInput[1]);
        size = Integer.parseInt(splitInput[2]);
        int[][] matrix = ReadMatrix.run(Integer.toString(size));
        //System.out.println(Arrays.deepToString(matrix));
        runTest(matrix);
    }

    private static void runTest(int[][] matrix) {
        List<Double> sequentialDurations = new ArrayList<>();
        List<Double> parallelDurations = new ArrayList<>();

        // Run sequential tests
        for (int i = 0; i < amountOfTests; i++) {
            long startS = System.nanoTime();
            int[][] sequentialResult = multiplyMatricesSequential(matrix, matrix);
            long endS = System.nanoTime();
            sequentialDurations.add(printAndCalcDuration(startS, endS, sequentialResult));
        }
        for (int i = 0; i < amountOfTests; i++) {
            long startP = System.nanoTime();
            int[][] parallelResult = multiplyMatricesParallel(matrix, matrix);
            long endP = System.nanoTime();
            parallelDurations.add(printAndCalcDuration(startP, endP, parallelResult));
        }

        csvWriter(sequentialDurations, "sequential");
        csvWriter(parallelDurations, "parallel");
    }

    private static double printAndCalcDuration(long startTime, long endTime, int[][] resultMatrix) {
        System.out.println(resultMatrix[0][0]);
        return (endTime-startTime) / 1.0E9;
    }

    private static void csvWriter(List<Double> durations, String name) {
        try (FileWriter csvWriter = new FileWriter(name + ".csv")) {
            for (int i = 0; i < durations.size(); i++) {
                csvWriter.append(Integer.toString(i+1));
                csvWriter.append(", ");
                csvWriter.append(Double.toString(durations.get(i)));
                csvWriter.append("\n");
            }
            csvWriter.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static int[][] multiplyMatricesParallel(int[][] matrixA, int[][] matrixB) {
        int[][] matrixC = new int[size][size];

        ForkJoinPool customThreadPool = new ForkJoinPool(threads);
        customThreadPool.submit(() ->
                IntStream.range(0, size).parallel().forEach(i ->
                        multiplyMatrices(matrixA, matrixB, matrixC, size, i)
                )
        ).join();

        customThreadPool.shutdown();

        return matrixC;
    }

    public static int[][] multiplyMatricesSequential(int[][] matrixA, int[][] matrixB) {
        int[][] matrixC = new int[size][size];

        for(int i = 0; i < size; i++) {
            multiplyMatrices(matrixA, matrixB, matrixC, size, i);
        }

        return matrixC;
    }

    private static void multiplyMatrices(int[][] matrixA, int[][] matrixB, int[][] matrixC, int size, int row) {
        for (int j = 0; j < size; j++) {
            for (int k = 0; k < size; k++) {
                matrixC[row][j] += matrixA[row][k] * matrixB[k][j];
            }
        }
    }
}