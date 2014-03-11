

public class Customer {

	private String _name;
	private Vector _rentals = new Vector();
	
	public Customer(String name){
		_name = name
	}
	
	public int addRental(Rental arg){
		_rentals.addElement(arg);
	}
	
	public String getName(){
		return _name;
	}
	
}
