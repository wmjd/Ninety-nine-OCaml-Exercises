let rec gcd a b = match b with
	| 0 -> a
	| b -> gcd b (a mod b)
