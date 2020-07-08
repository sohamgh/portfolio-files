//Soham Ghose (1663396)
//CSE 143 AG HW 6
//This Class provides functions to create anagrams of a user specified phrase. 

import java.util.* ; 
public class AnagramSolver{
   //This field stores a inventory of the letters contained by each word in a dictionary
   private Map<String, LetterInventory> map ;
   //This field stores a list of words in a dictionary
   private List<String> dictionary  ;  
   //constructor 
   //takes in a list of words as a dictionary 
   //stores the words in another dictionary and links the words to an inventory 
   //of the letters they contain. 
   public AnagramSolver(List<String> dict){
      dictionary = dict ; 
      map = new HashMap<String, LetterInventory>() ; 
      for(String word : dictionary){
         map.put(word, new LetterInventory(word)); 
      }  
   }
   
   //takes in a phrase and an integer representing 
   //the maximum number of words client want on each anagram. 
   //throws IllegalArgumentException if the input integer is less than 0. 
   //print all anagrams with the maximum number of words
   //prints all possible anagrams if input integer is equal to 0. 
   public void print(String s, int max){
      if(max < 0){
         throw new IllegalArgumentException() ; 
      }
      LetterInventory phrase = new LetterInventory(s) ;
      List<String> reducedDict = new ArrayList<String>() ; 
      
      for(String word : dictionary){ 
     LetterInventory check = phrase.subtract(map.get(word)) ; 
         if(check != null){
            reducedDict.add(word); 
         }
      }
      List<String> words = new ArrayList<String>() ; 
      print(phrase, max, words, reducedDict) ; 
   }
   
   //takes in the letter inventory of client input phrase, a dictionary, an empty list 
   //and the maximum number of words for each anagram
   //prints anagrams of given phrase based on the same criteria as parent method. 
   private void print(LetterInventory phrase , int max, List<String> words, List<String> reducedDict){
      if((phrase.size() == 0 && words.size() <= max) || (phrase.size() == 0 && max == 0)){
         System.out.println(words); 
      }
      else{
         for(String word : reducedDict){
            LetterInventory reduced  = phrase.subtract(map.get(word)); 
            if(reduced != null){
              words.add(word); 
              print(reduced, max, words, reducedDict); 
              words.remove(word); 
            }
         }
      }   
   }
}