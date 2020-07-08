// why is C 3 
//what is the order of strings

import java.util.* ;
import java.io.* ;
import java.math.* ; 
// fix E issue 
public class GuitarString{
   private Queue<Double> RingBuffer ;
   private int N; 
      public GuitarString(double frequency){
      int smpl = StdAudio.SAMPLE_RATE ; 
      N = (int)(smpl/frequency) ; 
      RingBuffer = new LinkedList<Double>() ;
      if( N < 2 || frequency == 0 ){
         throw new IllegalArgumentException() ;  
      }
      for(int i = 0 ; i < N ; i++){
         RingBuffer.add(0.0) ;    
      }
   }
   
   public GuitarString(double[] init ){
      if( init.length < 2 ){
         throw new IllegalArgumentException() ; 
      }
      for(int i = 0 ; i < init.length ; i++){
         RingBuffer.add(init[i]) ;
      }
   }
   //change to same loop because its a queue 
   public void pluck(){
      int N = RingBuffer.size() ;
      while(!(RingBuffer.isEmpty())){
         RingBuffer.remove();
      }
      for(int i = 0 ; i < N ; i++ ){
         Random randy = new Random() ;
         RingBuffer.add(randy.nextDouble() - 0.5) ;   
      }
   }
   
   public void tic(){
      for(int i = 0 ; i < N ; i++){
         double sample = RingBuffer.remove(); 
         double second = RingBuffer.peek(); 
         RingBuffer.add(0.996 * (sample + second)/2); 
      }         
   }
   
   public double sample(){
      return RingBuffer.peek() ;  
   }
}