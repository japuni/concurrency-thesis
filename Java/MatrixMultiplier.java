import java.util.Arrays;

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

        long start = System.nanoTime();
        int[][] matrixC = multiplyMatrices(matrixA, matrixB);
        long end = System.nanoTime();

        double duration = (end-start) / 1.0E9;
        double operations = 8000000000.0;
        double throughput = (operations / duration) / 1000;

        System.out.println(Arrays.deepToString(matrixC));
        System.out.println("Computation time: " + (end-start)/1.0E9);
        System.out.println("Throughput: " + throughput + " kOp/s");
    }

    public static int[][] multiplyMatrices(int[][] matrixA, int[][] matrixB) {
        int[][] matrixC = new int[SIZE][SIZE];
        int n = SIZE;
        int p = SIZE;
        int m = SIZE;

        for(int i = 0; i < n; i++) {
            for (int j = 0; j < p; j++) {
                int sum = 0;
                for (int k = 0; k < m; k++) {
                    sum += matrixA[i][k] * matrixB[k][j];
                }
                matrixC[i][j] = sum;
            }
        }

        /*

            for every row in MatrixA {
                for every column in MatrixB {
                    for every column in MatrixA and row in MatrixB {
                        multiply the values and add to sum
                    }
                    add sum to MatrixC for current row and column
                }
            }


            A = n x m -> A[n][m]
            B = m x p -> B[m][p]

            C = n x p

            [|0,0|, |0,1|]
            [|1,0|, |1,1|]

            [|0,0|, |0,1|]
            [|1,0|, |1,1|]
            X, Y - X ökar höger, Y ökar neråt
         */
        return matrixC;
    }
}
