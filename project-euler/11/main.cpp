#include <iostream>
#include "read_grid_from_file.h"

int main()
{
	std::vector<int> grid;
	
	grid = read_Grid_From_File("20x20_number_grid.txt");
	
	long long int max_product = 0;
	
	//horizontal max
	for (int i = 0; i < 20*20; i += 20) {
		for(int j = 0; j < 17; ++j) {
			if(max_product < grid[i+j]*grid[i+j+1]*grid[i+j+2]*grid[i+j+3])
				max_product = grid[i+j]*grid[i+j+1]*grid[i+j+2]*grid[i+j+3];
		}
	}
	
	//vertical max
	for (int i = 0; i < 17*20; i += 20) {
		for(int j = 0; j < 20; ++j) {
			if(max_product < grid[i+j]*grid[(i+20)+j]*grid[(i+40)+j]*grid[(i+60)+j])
				max_product = grid[i+j]*grid[(i+20)+j]*grid[(i+40)+j]*grid[(i+60)+j];
		}
	}
	
	//diagonal max
	for (int i = 0; i < 17*20; i += 20) {
		for(int j = 0; j < 17; ++j) {
			if(max_product < grid[i+j]*grid[(i+20)+j+1]*grid[(i+40)+j+2]*grid[(i+60)+j+3])
				max_product = grid[i+j]*grid[(i+20)+j+1]*grid[(i+40)+j+2]*grid[(i+60)+j+3];
		}
	}
	
	for (int i = 0; i < 17*20; i += 20) {
		for(int j = 3; j < 20; ++j) {
			if(max_product < grid[i+j]*grid[(i+20)+j-1]*grid[(i+40)+j-2]*grid[(i+60)+j-3])
				max_product = grid[i+j]*grid[(i+20)+j-1]*grid[(i+40)+j-2]*grid[(i+60)+j-3];
		}
	}
	
	std::cout << "The maximum product is: " << max_product;
	
	return 0;
}