public class GeoLocationClient {
	public static void main(String[] args) {
		// GeoLocation objects to store the latitudes and longitudes of the stash, studio and FBI building
	
      GeoLocation WaltersStash = new GeoLocation(34.988889, -106.614444);
		GeoLocation wbstudio = new GeoLocation(34.989978, -106.614357);
		GeoLocation fbiBuilding = new GeoLocation(35.131281, -106.61263);
		
		// Printing out the coordinates of the stash, studio and FBI building using the GeoLocation objects
		System.out.println("the stash is at " + WaltersStash.toString());
		System.out.println("ABQ studio is at " + wbstudio.toString());
		System.out.println("FBI building is at " + fbiBuilding.toString());
		
		System.out.println("distance in miles between:");
      getDistanceFromStash(WaltersStash , wbstudio, "studio");
      getDistanceFromStash(WaltersStash, fbiBuilding, "fbi");
			}
         
       // calculates and prints the distance between stash and the secondLocation by taking in the second location
       // string, and the two geolocations as parameters
   public static void getDistanceFromStash(GeoLocation stash , GeoLocation x, String secondLocation){
      System.out.println("	stash/"+ secondLocation + " = " + stash.distanceFrom(x));
   


   }
}