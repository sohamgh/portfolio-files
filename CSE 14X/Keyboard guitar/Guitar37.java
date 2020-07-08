// This is a sample class that implements the Guitar interface.  It is not well
// documented.
// constant for energy decay factor 
// thrown exceptions 

public class Guitar37 implements Guitar {
    private GuitarString[] gg = new GuitarString[37];
    public static final String KEYBOARD = "q2we4r5ty7u8i9op-[=zxdcfvgbnjmk,.;/' " ;  // keyboard layout

    // create two guitar strings, for concert A and C
    public Guitar37() {
        double CONCERT_A = 440.0;
        //double CONCERT_C = CONCERT_A * Math.pow(2, 3.0/12.0);  
        gg[0] = new GuitarString(CONCERT_A);
        for(int i = 1 ; i < 37 ; i++){
            double Concert = CONCERT_A * Math.pow(2, (KEYBOARD.indexOf(i)-24)/12.0);
            gg[i] = new GuitarString(Concert) ;   
        }
        //stringC = new GuitarString(CONCERT_C);
    }

    public void playNote(int pitch) {
       int index = pitch + 24 ; 
       gg[index].pluck() ;     
    }           

    public boolean hasString(char string) {
      return KEYBOARD.indexOf(string) > gg.length ;            
    }
    
    public void pluck(char string) {
        gg[KEYBOARD.indexOf(string)].pluck() ; 
    }
   //done
    public double sample() {
    double sum = 0 ; 
      for(int i = 0 ; i < gg.length ; i++){
         sum += gg[i].sample();
      }
      return sum ; 
    }
   //done
    public void tic() {
        for(int i = 0 ; i < gg.length ; i++){
         gg[i].tic();
      }
    }

    public int time() {
        return -1;  // not implemented
    }
}