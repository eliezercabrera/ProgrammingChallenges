#include <iostream>
#include <fstream>
#include <vector>
#include "../whatever.h"
#include <string>
#include <cstdlib>

using namespace std;

vector<long long> read_Chop_Digit(string s)	//reads from file, takes 10 most significant digits, converts string to number
{
	vector<string> ten_digit_lines;
	vector<long long> ten_digit_numbers;
	ifstream ifs(s.data(), std::ifstream::in);
	
	string current_line;
	while (getline(ifs, current_line))
		ten_digit_lines.push_back(current_line.substr(0, 15));
	
	for (string i: ten_digit_lines)
		ten_digit_numbers.emplace_back( std::atoll(i.data()) );
	
	return ten_digit_numbers;
}

int main()
{
	whatever::StopWatch measure;
	
	long long sum = 0;
	vector<long long> data = read_Chop_Digit("addends.txt");
	
	for (long long i: data) {
		sum += i;
		cerr << "Sum: " << sum << "\tData: " << i << '\n';
	}
	
	auto elapsed_us = measure.elapsedMs().count();
	
	cout << "The most significant digits of the sum are: " << sum
		 << "\nIt took " << elapsed_us << " milliseconds to compute.\n";

	return 0;
}