#include <iostream>
#include <vector>
#include <cassert>
#include <algorithm>
#include <random>
#include <thread>
#include <future>
#include <chrono>

int n = 100;

/**
 * Characteristic function for the glove game
*/
template<class RandomIt>
double v(RandomIt first, RandomIt last) {
    int left_count = 0, right_count = 0;

    for (auto player = first; player < last; player++) {
        if (*player <= n / 2) left_count++;
        else right_count++;
    }

    return std::min(left_count, right_count);
}

/*std::vector<int> sample_from(const std::vector<int>& population) {
    std::vector<int> result;
    std::sample(population.begin(), population.end(), std::back_inserter(result),
                3, std::mt19937{std::random_device{}()});

    return result;
}*/

template <class T>
void print_vector(const std::vector<T>& v) {
    std::cout << '[';
    for (auto& i : v)
        std::cout << i << ' ';
    std::cout << ']' << std::endl;
}

/**
 * Gets the shapley value for each player. The shapley values are not yet divided by sample_count.
*/
std::vector<double> get_shapley_value_sums(const std::vector<int>& population, int sample_count) {
    std::random_device rd;
    std::mt19937 g(rd());

    // shapley_values[player_index]
    std::vector<double> shapley_values(population.size(), 0.0);

    // every thread gets a copy of population
    auto order = std::vector<int>(population);

    //std::cout << "Starting calculation in thread...\n";

    for (int i = 0; i < sample_count; i++) {
        std::shuffle(order.begin(), order.end(), g);
        for (int j = 0; j < n; j++) {
            double marginal_contribution = v(order.begin(), order.begin() + j + 1) - v(order.begin(), order.begin() + j);
            int player = order[j];
            shapley_values[player - 1] += marginal_contribution;
        }
    }

    return shapley_values;
}

void with_threads(const std::vector<int>& population, int sample_count) {
    const int thread_count = 32;

    int sample_count_per_thread = sample_count / thread_count;

    std::vector<std::future<std::vector<double>>> futures;

    for (int i = 0; i < thread_count; i++) {
        auto future = std::async(std::launch::async, get_shapley_value_sums, population, sample_count_per_thread);
        futures.push_back(std::move(future));
    }

    // shapley_values[player_index]
    std::vector<double> shapley_values(population.size(), 0.0);

    for (auto& future : futures) {
        auto shapley_values_sums = future.get();
        for (int i = 0; i < shapley_values.size(); i++) {
            shapley_values[i] += shapley_values_sums[i];
        }
    }

    for (int i = 0; i < shapley_values.size(); i++)
        shapley_values[i] /= sample_count;

    print_vector(shapley_values);
}

void without_threads(const std::vector<int>& population, int sample_count) {
    std::random_device rd;
    std::mt19937 g(rd());

    // shapley_values[player_index]
    std::vector<double> shapley_values(n, 0.0);

    auto order = std::vector<int>(population);

    for (int i = 0; i < sample_count; i++) {
        std::shuffle(order.begin(), order.end(), g);
        for (int j = 0; j < n; j++) {
            double marginal_contribution = v(order.begin(), order.begin() + j + 1) - v(order.begin(), order.begin() + j);
            int player = order[j];
            shapley_values[player - 1] += marginal_contribution;
        }
    }

    for (int i = 0; i < shapley_values.size(); i++)
        shapley_values[i] /= sample_count;

    print_vector(shapley_values);
}

int main(int argc, char** argv)
{
    assert(n % 2 == 0);

    const int sample_count = 10'000'000; // m

    std::vector<int> population;
    for (int player = 1; player <= n; player++)
        population.push_back(player);

    std::cout << "With threads:" << std::endl;
    auto start = std::chrono::high_resolution_clock::now();
    with_threads(population, sample_count);
    auto stop = std::chrono::high_resolution_clock::now();

    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);
    std::cout << duration.count() << "ms" << std::endl;

    std::cout << "Without threads:" << std::endl;
    start = std::chrono::high_resolution_clock::now();
    without_threads(population, sample_count);
    stop = std::chrono::high_resolution_clock::now();

    duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);
    std::cout << duration.count() << "ms" << std::endl;

    return 0;
}