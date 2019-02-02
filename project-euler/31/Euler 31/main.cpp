#include <iostream>
#include <vector>

using namespace std;
const vector<int> coins = { 200, 100, 50, 20, 10, 5, 2, 1 };


int waysToMakeChange(const vector<int>& denominations, const int target_change)
{
	vector<vector<int>> table(denominations.size() + 1, vector<int>(target_change+1));

	for (vector<int>& i : table)
	{
		i[0] = 1;
	}

	for (unsigned int i = 1; i <= denominations.size(); ++i)
	{
		for (unsigned int j = 1; j <= target_change; ++j)
		{
			for (unsigned int k = 0; k <= j / denominations[i-1]; ++k)
			{
				table[i][j] += table[i - 1][j - k*denominations[i - 1]];
			}
		}
	}

	/*
	for (const vector<int>& i : table)
	{
		for (int j : i)
		{
			cout << j << ' ';
		}
		cout << endl;
	}
	*/

	return table.back().back();
}

int findposs(const int money, const int max_coin_index)
{
	int sum = 0;

	if (max_coin_index == 7)
	{
		return 1;
	}

	for (unsigned int i = max_coin_index; i < 8; ++i)
	{
		if (money - coins[i] == 0)
		{
			sum += 1;
		}
		else if (money - coins[i] > 0)
		{
			sum += findposs(money - coins[i], i);
		}
	}
	return sum;
}

int main()
{
	const vector<int> DENOMINATIONS = { 1, 2, 5, 10, 20, 50, 100, 200 };
	const int TARGET_VALUE = 200;

	cout << waysToMakeChange(DENOMINATIONS, TARGET_VALUE) << endl << endl;

	cout << findposs(200, 0) << endl << endl;

	char c;
	cin >> c;

	return 0;
}