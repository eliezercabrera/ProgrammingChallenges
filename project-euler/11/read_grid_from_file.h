#include <fstream>
#include <vector>

std::vector<int> read_Grid_From_File(std::string s)
{
	std::ifstream ifs (s.data(), std::ifstream::in);
	std::vector<int> grid;
	
	int current_int;	
	while(ifs >> current_int)
		grid.push_back(current_int);
		
	return grid;
}