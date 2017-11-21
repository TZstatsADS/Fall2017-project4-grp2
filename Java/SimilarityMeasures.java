import java.util.*;
import java.io.*;
//import org.apache.commons.math3.stat.correlation.*;//SpearmansCorrelation;

public class SimilarityMeasures {

    public static final void main(String[] args) throws FileNotFoundException{
        
        // string to be analysed
        // create File objects
        File inFile  = new File( args[0] );
        int nrow = Integer.parseInt( args[1] );
        int ncol = Integer.parseInt( args[2] );

        try{

            // instantiate matrix to perform calculation
        		double[][] dataMatrix = new double[ ncol-1 ][ nrow-1 ];
            String[] rowOfMatrix = new String[ncol];

            Scanner scanner = new Scanner( inFile );
            scanner.useDelimiter(",");

            // read header line
            //String[] header = new String[ncol];
            //header = scanner.nextLine().split(",");

            int i = 0;
            // while there's data in line
            while(scanner.hasNext()){
                
                rowOfMatrix = scanner.nextLine().split(",");

                i++;
                for( int j = 1; j < rowOfMatrix.length; j++ ){
                	
                		Double val = Double.parseDouble(rowOfMatrix[j]);
                    dataMatrix[j-1][i-1] = (double) val;

                }

            }
            scanner.close();        

            // print original matrix dimensions
            System.out.println("original data dimensions");
            System.out.println( "nrows = " + dataMatrix.length );
            System.out.println( "ncols = " + dataMatrix[0].length );
            System.out.println("");

            // transpose matrix
            double[][] tDataMatrix = transposeMatrix( dataMatrix );

            // print transposed matrix dimensions
            System.out.println("transpose data dimensions");
            System.out.println( "nrows = " + tDataMatrix.length );
            System.out.println( "ncols = " + tDataMatrix[0].length );
            System.out.println("");

            // compute spearman correlation coeffcient
            System.out.println("compute spearman correlation coeffcient");
            
            // instantiate object SpearmansCorrelation
            //SpearmansCorrelation sc = new SpearmansCorrelation();
            //double cor = sc.correlation( dataMatrix[0], dataMatrix[1]);
            //RealMatrix m = new sc.computeCorrelationMatrix( tDataMatrix );
            
            //System.out.println("corr value = " + dataMatrix[0]);

            System.out.println("done!");

            //computeCorrelationMatrix(double[][] matrix)



        }
        catch(FileNotFoundException e){

            System.out.println("Exception: " + e);

        }


    }

    // method to tranpose matrix
    public static double[][] transposeMatrix(double[][] matrix)
    {

        int m = matrix.length;
        int n = matrix[0].length;

        double[][] transposedMatrix = new double[n][m];

        for(int x = 0; x < n; x++)
        {
            for(int y = 0; y < m; y++)
            {
                transposedMatrix[x][y] = matrix[y][x];
            }
        }

        return transposedMatrix;
    }
     

}
