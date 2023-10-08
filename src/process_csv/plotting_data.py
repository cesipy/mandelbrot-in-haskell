import matplotlib.pyplot as plt
import csv
import os

def read_in_from_csv(path):
    result = []
    with open(path, mode="r") as f:
        
        csv_file = csv.reader(f)

        for lines in csv_file:
            result.append(lines)


    return result

def process_data(csv_list):
    x_values = []
    y_values = []

    for i in csv_list:
        x_values.append(float(i[0]))
        y_values.append(float(i[1]))


    return x_values, y_values

def plot_data(x_arr, y_arr):
    plt.scatter(x_arr, y_arr)
    plt.xlabel('y')
    plt.ylabel('x')
    plt.title('mandelbrot')

    plt.xlim(-2, 2)
    plt.ylim(-2, 2)

    plt.grid(True)
    plt.show()


def main():
   # os.chdir("..")
    returning = read_in_from_csv("results.csv")
    
    x_values, y_values = process_data(returning)

    plot_data(x_values, y_values)
   

main()