// Soham Ghose (1663396)
// CSE 143 section AG HW 7
// This program manages a game of 20 questions

import java.io.* ;
import java.util.* ; 

public  class QuestionTree{
   // questionNode field that stores the overall root object 
   private QuestionNode root ; 
   // field stores the Scanner used to take in user input from console. 
   private Scanner console ; 
   // constructor
   // sets the overall tree root to "computer"
   public  QuestionTree(){
      root = new QuestionNode("computer") ; 
      console = new Scanner(System.in) ;         
   }
   // takes in a scanner linked to the required file
   // reads questions and answer guesses from a client specified file
   public  void read(Scanner input){
      root = readfile(input) ;           
   }
   // takes in a printstream to write to file
   // writes user input questions and objects to a file
   public  void write(PrintStream output){
      writeToFile(output, root) ; 
   }
   // asks question to the user to guess the correct object. 
   // adds new object of user choice in case all guesses are incorrect
   // adds user specified question to the tree that leads to the new object
   public  void askQuestions(){
      root = newQuestion(root) ; 
   }
   // post: asks the user a question, forcing an answer of "y " or "n";
   // returns true if the answer was yes, returns false otherwise
   public boolean yesTo(String prompt){
       System.out.print(prompt + " (y/n)? ");
       String response = console.nextLine().trim().toLowerCase();
       while (!response.equals("y") && !response.equals("n")){
           System.out.println("Please answer y or n.");
           System.out.print(prompt + " (y/n)? ");
           response = console.nextLine().trim().toLowerCase();
       }
       return response.equals("y");
   } 
   // takes in a scanner linked to required file 
   // reads the file linked to scanner and stores it. 
   private QuestionNode readfile(Scanner input){
      if(input.nextLine().equals("A:")){
         return new QuestionNode(input.nextLine()) ; 
      }
      else{
         return new QuestionNode(input.nextLine() , readfile(input), readfile(input)) ;
      }
   }
   // takes in a printstream and a parent questionNode 
   // returns null if input node is null
   // writes currently stores tree to a new file 
   private void writeToFile(PrintStream output , QuestionNode node){
      if (node == null){
         return;
      } 
      else if(node.isLeaf){
         output.println("A:");
         
         output.println(node.data);
      } 
      else{
         output.println("Q:");
         output.println(node.data);
      }
      writeToFile(output, node.left);
      writeToFile(output, node.right);
   }
   // takes in a questionNode
   // asks if guess is correct
   // if guess is incorrect, adds new questionNodes to existing tree in preorder. 
   private QuestionNode newQuestion(QuestionNode node){
      if(node.isLeaf){
         if(yesTo("Would your object happen to be " + node.data + "?")){
            System.out.println("Great, I got it right"); 
         }
         else{
            System.out.print("What is the name of your object? ") ;
            QuestionNode object = new QuestionNode(console.nextLine()) ;
            System.out.println("please give me a yes/no question that ");
            System.out.println("distinguishes between your object ");
            System.out.print("and mine --> ");
            String newQuestion = console.nextLine() ;
            QuestionNode current = node ; 
            if(yesTo("And what is the answer to your object")){
               node = new QuestionNode(newQuestion, object, current ); 
            } 
            else{
               node = new QuestionNode(newQuestion, current, object);
            }  
         }
      } 
      else{
         if(yesTo(node.data)){
            node.left = newQuestion(node.left) ; 
         }
         else{
            node.right = newQuestion(node.right) ; 
         }
      } 
      return node ;  
   }
   
   
}