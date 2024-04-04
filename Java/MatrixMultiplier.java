package Java;

import java.util.Arrays;
import java.util.concurrent.ForkJoinPool;
import java.util.stream.IntStream;

public class MatrixMultiplier {

    public static final int SIZE = 2000;

    public static void main(String[] args) {
        int size = SIZE;
        int[][] matrixA = new int[size][size];
        int[][] matrixB = new int[size][size];

        java.util.Random random = new java.util.Random();

        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                matrixA[i][j] = random.nextInt(100); // Generate random numbers between 0 to 99
                matrixB[i][j] = random.nextInt(100); // Generate random numbers between 0 to 99
            }
        }

        long startS = System.nanoTime();
        int[][] matrixCSequential = multiplyMatricesSequential(matrixA, matrixB);
        long endS = System.nanoTime();

        long startP = System.nanoTime();
        int[][] matrixCParallel = multiplyMatricesParallel(matrixA, matrixB);
        long endP = System.nanoTime();

        double operations = 8000000000.0;

        double durationSequential = (endS-startS) / 1.0E9;
        double throughputSequential = (operations / durationSequential) / 1000;
        double durationParallel = (endP-startP) / 1.0E9;
        double throughputParallel = (operations / durationParallel) / 1000;

        System.out.println(Arrays.deepToString(matrixCParallel));
        System.out.println("Computation time (sequential): " + durationSequential);
        System.out.println("Throughput (sequential): " + throughputSequential + " kOp/s");
        System.out.println("Computation time (parallel): " + durationParallel);
        System.out.println("Throughput (parallel): " + throughputParallel + " kOp/s");
    }

    public static int[][] multiplyMatricesParallel(int[][] matrixA, int[][] matrixB) {
        int[][] matrixC = new int[SIZE][SIZE];

        ForkJoinPool customThreadPool = new ForkJoinPool(6);
        customThreadPool.submit(() -> {
            IntStream.range(0, SIZE).parallel().forEach
                    (i -> {
                        for (int j = 0; j < SIZE; j++) {
                            for (int k = 0; k < SIZE; k++) {
                                matrixC[i][j] += matrixA[i][k] * matrixB[k][j];
                            }
                        }
                    });
        }).join();
        return matrixC;
    }

    public static int[][] multiplyMatricesSequential(int[][] matrixA, int[][] matrixB) {
        int[][] matrixC = new int[SIZE][SIZE];

        for(int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                int sum = 0;
                for (int k = 0; k < SIZE; k++) {
                    sum += matrixA[i][k] * matrixB[k][j];
                }
                matrixC[i][j] = sum;
            }
        }

        return matrixC;
    }
}
