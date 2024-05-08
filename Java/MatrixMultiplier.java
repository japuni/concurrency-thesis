package Java;

import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;
import java.util.concurrent.ForkJoinPool;
import java.util.stream.IntStream;
// Set backlog to the same as erlang
public class MatrixMultiplier {

    private static int amountOfTests;
    private static int size;
    private static int threads;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);
        System.out.print("SIZE, THREADS, AMOUNT OF TESTS -> ");
        String input = scanner.nextLine();
        String[] splitInput = input.split(", ");
        size = Integer.parseInt(splitInput[0]);
        threads = Integer.parseInt(splitInput[1]);
        amountOfTests = Integer.parseInt(splitInput[2]);
        List<int[][]> matrices = createMatricies();
        runTest(matrices);
    }

    private static void runTest(List<int[][]> matrices) throws IOException {
        int[][] matrixA = matrices.get(0);
        int[][] matrixB = matrices.get(1);

        List<Double> sequentialDurations = new ArrayList<>();
        List<Double> parallelDurations = new ArrayList<>();

        //Sequential tests
        for (int i = 0; i < amountOfTests; i++) {
            long startS = System.nanoTime();
            int[][] matrixCSeq = multiplyMatricesSequential(matrixA, matrixB);
            long endS = System.nanoTime();
            System.out.println(matrixCSeq[0][0]);
            double durationSequential = (endS-startS) / 1.0E9;
            sequentialDurations.add(durationSequential);
        }
        csvWriter(sequentialDurations, "sequential");

        //Parallel tests
        for (int i = 0; i < amountOfTests; i++) {
            long startP = System.nanoTime();
            int[][] matrixCPar = multiplyMatricesParallel(matrixA, matrixB);
            long endP = System.nanoTime();
            System.out.println(matrixCPar[0][0]);
            double durationParallel = (endP-startP) / 1.0E9;
            parallelDurations.add(durationParallel);
        }
        csvWriter(parallelDurations, "parallel");
    }

    private static List<int[][]> createMatricies() {
        int[][] matrixA = new int[size][size];
        int[][] matrixB = new int[size][size];
        List<int[][]> matrices = new LinkedList<>();

        java.util.Random random = new java.util.Random();

        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                matrixA[i][j] = random.nextInt(100);
                matrixB[i][j] = random.nextInt(100);
            }
        }

        matrices.add(matrixA);
        matrices.add(matrixB);

        return matrices;
    }

    private static void csvWriter(List<Double> durations, String name) throws IOException {
        FileWriter csvWriter = new FileWriter(name + ".csv");
        for (int i = 0; i < durations.size(); i++) {
            csvWriter.append(Integer.toString(i+1));
            csvWriter.append(", ");
            csvWriter.append(Double.toString(durations.get(i)));
            csvWriter.append(", ");
            csvWriter.append(Integer.toString(threads));
            csvWriter.append("\n");
        }
        csvWriter.flush();
        csvWriter.close();
    }

    public static int[][] multiplyMatricesParallel(int[][] matrixA, int[][] matrixB) {
        int[][] matrixC = new int[size][size];

        ForkJoinPool customThreadPool = new ForkJoinPool(threads);
        customThreadPool.submit(() -> IntStream.range(0, size).parallel().forEach(i -> {
            for (int j = 0; j < size; j++) {
                for (int k = 0; k < size; k++) {
                    matrixC[i][j] += matrixA[i][k] * matrixB[k][j];
                }
            }
        })).join();

        customThreadPool.shutdown();
        return matrixC;
    }

    public static int[][] multiplyMatricesSequential(int[][] matrixA, int[][] matrixB) {
        int[][] matrixC = new int[size][size];

        for(int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                for (int k = 0; k < size; k++) {
                    matrixC[i][j] += matrixA[i][k] * matrixB[k][j];
                }
            }
        }
        return matrixC;
    }
}
