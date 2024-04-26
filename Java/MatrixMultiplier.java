package Java;

import java.util.Arrays;
import java.util.concurrent.ForkJoinPool;
import java.util.stream.IntStream;

public class MatrixMultiplier {

    public static final int SIZE = 1000;

    public static void main(String[] args) {
        int size = SIZE;

        int[][] matrixA = new int[size][size];
        int[][] matrixB = new int[size][size];

        java.util.Random random = new java.util.Random();

        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                matrixA[i][j] = random.nextInt(100);
                matrixB[i][j] = random.nextInt(100);
            }
        }

        long startS = System.nanoTime();
        int[][] matrixCSeq = multiplyMatricesSequential(matrixA, matrixB);
        long endS = System.nanoTime();

        long startP = System.nanoTime();
        int[][] matrixCPar = multiplyMatricesParallel(matrixA, matrixB, 48);
        long endP = System.nanoTime();

        double operations = Math.pow(SIZE, 3);

        double durationSequential = (endS-startS) / 1.0E9;
        double throughputSequential = (operations / durationSequential) / 1000;
        double durationParallel = (endP-startP) / 1.0E9;
        double throughputParallel = (operations / durationParallel) / 1000;

        System.out.println("Computation time (sequential): " + durationSequential);
        System.out.println("Throughput (sequential): " + throughputSequential + " kOp/s");
        System.out.println("Computation time (parallel): " + durationParallel);
        System.out.println("Throughput (parallel): " + throughputParallel + " kOp/s");
    }

    public static int[][] multiplyMatricesParallel(int[][] matrixA, int[][] matrixB, int threads) {
        int[][] matrixC = new int[SIZE][SIZE];

        ForkJoinPool customThreadPool = new ForkJoinPool(threads);
        customThreadPool.submit(() -> IntStream.range(0, SIZE).parallel().forEach(i -> {
            for (int j = 0; j < SIZE; j++) {
                for (int k = 0; k < SIZE; k++) {
                    matrixC[i][j] += matrixA[i][k] * matrixB[k][j];
                }
            }
        })).join();

        customThreadPool.shutdown();
        return matrixC;
    }

    public static int[][] multiplyMatricesSequential(int[][] matrixA, int[][] matrixB) {
        int[][] matrixC = new int[SIZE][SIZE];

        for(int rowA = 0; rowA < SIZE; rowA++) {
            for (int columnB = 0; columnB < SIZE; columnB++) {
                int sum = 0;
                for (int columnArowB = 0; columnArowB < SIZE; columnArowB++) {
                    sum += matrixA[columnArowB][rowA] * matrixB[columnB][columnArowB];
                }
                matrixC[columnB][rowA] = sum;
            }
        }

        return matrixC;
    }
}
