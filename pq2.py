"""
Kyle Maclean
Tests for Priority Queues
December 2021
"""

import math
import random
import matplotlib.pyplot as plt
from unittest import TestCase

import p3

valid_uniparental_heaps = [
    [],
    [1],
    [1, 2],
    [1, 2, 3],
    [1, 3, 2, 4],
    [1, 3, 7, 8, 5],
    [4, 5, 19, 20, 8],
    [1, 2, 2, 3, 5, 5, 5],
    [2, 4, 19, 5, 8, 22, 20],
    [2, 4, 19, 5, 8, 22, 20, 9],
    [8, 12, 9, 60, 19, 11, 17, 99],
    [5, 8, 9, 12, 19, 11, 17, 99, 60]
]

invalid_uniparental_heaps = [
    [2, 1],
    [2, 1, 3],
    [1, 4, 2, 3, 5],
    [1, 4, 2, 10, 12, 44, 55, 9]
]

valid_biparental_heaps = [
    [],
    [10],
    [9, 10],
    [8, 10, 9],
    [7, 8, 9, 10],
    [6, 8, 7, 10, 9],
    [5, 8, 6, 10, 9, 7],
    [4, 5, 6, 8, 9, 7, 10],
    [3, 5, 4, 8, 6, 7, 10, 9],
    [2, 5, 3, 8, 6, 4, 10, 9, 7],
    [1, 5, 2, 8, 6, 3, 10, 9, 7, 4],
    [7, 19, 12, 72, 27, 15, 88, 89, 35, 22],
    [8, 13, 9, 22, 17, 12, 23, 25, 22, 13, 25],
    [1, 5, 2, 8, 6, 3, 10, 9, 7, 4, 20, 17, 19, 18, 22, 23, 21]
]

invalid_biparental_heaps = [
    [10, 9],
    [5, 6, 4],
    [3, 6, 11, 5],
    [7, 8, 9, 10, 6],
    [10, 9, 8, 7, 6, 5, 11]
]

biparental_level_to_indices = {
    0: range(0, 1),
    1: range(1, 3),
    2: range(3, 6),
    3: range(6, 10),
    4: range(10, 15),
    5: range(15, 21),
    6: range(21, 28),
    7: range(28, 36),
    8: range(36, 45),
    9: range(45, 55),
    10: range(55, 66)
}


def get_time_bound(heap, complexity_class):
    if complexity_class == 'constant':
        return 1
    elif complexity_class == 'linear':
        return len(heap) + 1
    elif complexity_class == 'logarithmic':
        if len(heap) == 0:
            return 1
        return math.log(len(heap), 2) + 1
    elif complexity_class == 'square root':
        return (2 * len(heap)) ** (1 / 2) + 1
    else:
        raise ValueError('Unsupported complexity class.')


class UnitTests(TestCase):

    def test_min_correctness(self):
        self.assertEqual(None, p3.get_min([]))
        for heap in valid_uniparental_heaps + valid_biparental_heaps:
            if heap:
                self.assertEqual(min(heap), p3.get_min(heap))

    def test_min_efficiency(self):
        for heap in valid_uniparental_heaps + valid_biparental_heaps:
            _, time = p3.get_min_and_time(heap)
            self.assertLessEqual(time, get_time_bound(_, 'constant'))

    def test_max_correctness(self):
        parent_configurations = [
            [1, valid_uniparental_heaps],
            [2, valid_biparental_heaps]
        ]
        for config in parent_configurations:
            self.assertEqual(None, p3.get_max([], config[0]))
            for heap in config[1]:
                if heap:
                    self.assertEqual(max(heap), p3.get_max(heap, config[0]))

    def test_max_efficiency(self):
        parent_configurations = [
            [valid_uniparental_heaps, p3.get_uniparental_max_and_time, 'linear'],
            [valid_biparental_heaps, p3.get_biparental_max_and_time, 'square root']
        ]
        for config in parent_configurations:
            for heap in config[0]:
                _, time = config[1](heap)
                self.assertLessEqual(time, get_time_bound(heap, config[2]))

    def test_insert_correctness(self):
        parent_configurations = [
            [1],
            [2]
        ]
        for config in parent_configurations:
            for r in [range(20), reversed(range(20))]:
                heap = []
                for key in r:
                    expected_heap_length = len(heap) + 1
                    prior_key_in_heap = key in heap
                    p3.perform_insertion(heap, key, config[0])
                    actual_heap_length = len(heap)
                    post_key_in_heap = key in heap
                    self.assertTrue(p3.is_valid(heap, config[0]))
                    self.assertEqual(expected_heap_length, actual_heap_length)
                    self.assertTrue(prior_key_in_heap or post_key_in_heap)
            heap = []
            for _ in range(100):
                p3.perform_insertion(heap, random.randrange(-1000, 1000, 1), config[0])
                self.assertTrue(p3.is_valid(heap, config[0]))

    def test_insert_efficiency(self):
        parent_configurations = [
            [p3.perform_uniparental_ascending_swap_and_get_time, 'logarithmic'],
            [p3.perform_biparental_ascending_swap_and_get_time, 'square root']
        ]
        for config in parent_configurations:
            for r in [range(20), reversed(range(20))]:
                heap = []
                for key in r:
                    heap.append(key)
                    time = config[0](heap, len(heap) - 1, 1)
                    self.assertLessEqual(time, get_time_bound(heap, config[1]))

    def test_extract_correctness(self):
        parent_configurations = [
            [1, valid_uniparental_heaps],
            [2, valid_biparental_heaps]
        ]
        for config in parent_configurations:
            actual_min_key = p3.perform_extraction_and_get_min([], config[0])
            self.assertEqual(None, actual_min_key)
            for heap in config[1]:
                if heap:
                    expected_min_key = min(heap)
                    expected_heap_length = len(heap) - 1
                    actual_min_key = p3.perform_extraction_and_get_min(heap, config[0])
                    actual_heap_length = len(heap)
                    self.assertTrue(p3.is_valid(heap, config[0]))
                    self.assertEqual(expected_min_key, actual_min_key)
                    self.assertEqual(expected_heap_length, actual_heap_length)
            heap = []
            for n in [1, 2, 3, 4, 100]:
                for _ in range(n):
                    p3.perform_insertion(heap, random.randrange(-1000, 1000, 1), config[0])
                for _ in range(n):
                    expected_min = min(heap)
                    min_ = p3.perform_extraction_and_get_min(heap, config[0])
                    self.assertEqual(min_, expected_min)
                    self.assertTrue(p3.is_valid(heap, config[0]))

    def test_extract_efficiency(self):
        parent_configurations = [
            [valid_uniparental_heaps, 'logarithmic', p3.perform_uniparental_descending_swap_and_get_time],
            [valid_biparental_heaps, 'square root', p3.perform_biparental_descending_swap_and_get_time]
        ]
        for config in parent_configurations:
            for heap in config[0]:
                time_bound = get_time_bound(heap, config[1])
                if len(heap) == 1:
                    heap.pop()
                elif len(heap) > 1:
                    heap[0], heap[-1] = heap[-1], heap[0]
                    heap.pop(-1)
                    time = config[2](heap, 0, 1)
                    self.assertLessEqual(time, time_bound)

    def test_search_correctness(self):
        parent_configurations = [
            [valid_uniparental_heaps, 1],
            [valid_biparental_heaps, 2]
        ]
        for config in parent_configurations:
            for heap in config[0]:
                for key in range(30):
                    index = p3.search_for_key_and_get_index(heap, key, config[1])
                    if key not in heap:
                        self.assertIsNone(index)
                    else:
                        self.assertEqual(key, heap[index])

    def test_search_efficiency(self):
        parent_configurations = [
            [valid_uniparental_heaps, p3.uniparental_search_for_key_and_get_index_and_time, 'linear'],
            [valid_biparental_heaps, p3.biparental_search_for_key_and_get_index_and_time, 'square root']
        ]
        for config in parent_configurations:
            for heap in config[0]:
                for key in range(30):
                    time_bound = get_time_bound(heap, config[2])
                    if config[2] == 'square root':
                        # M,S: "... the cost for a search is at most 2 * sqrt(2 * n) + O(1) comparisons"
                        time_bound *= 2
                    _, time = config[1](heap, key)
                    self.assertLessEqual(time, time_bound)

    def test_is_valid_correctness(self):
        parent_configurations = [
            [1, valid_uniparental_heaps, invalid_uniparental_heaps],
            [2, valid_biparental_heaps, invalid_biparental_heaps]
        ]
        for config in parent_configurations:
            for heap in config[1]:
                self.assertTrue(p3.is_valid(heap, config[0]))
            for heap in config[2]:
                self.assertFalse(p3.is_valid(heap, config[0]))

    def test_get_biparental_level_correctness(self):
        for level in biparental_level_to_indices:
            for index in biparental_level_to_indices[level]:
                self.assertEqual(level, p3.get_biparental_level(index))

    def test_get_biparental_block_indices_correctness(self):
        for level in biparental_level_to_indices:
            l_edge_index = biparental_level_to_indices[level][0]
            r_edge_index = biparental_level_to_indices[level][-1]
            self.assertEqual((l_edge_index, r_edge_index), p3.get_biparental_block_indices(level))

    def test_plot_performance(self):
        parent_configuration = [
            {
                'parents': 1,
                'heap type': 'Uniparental Binary Heap',
                'insert function': p3.perform_uniparental_ascending_swap_and_get_time,
                'insert bound': 'logarithmic',
                'search function': p3.uniparental_search_for_key_and_get_index_and_time,
                'search bound': 'linear',
                'max function': p3.get_uniparental_max_and_time,
                'max bound': 'linear',
                'extract function': p3.perform_uniparental_descending_swap_and_get_time,
                'extract bound': 'logarithmic'
            },
            {
                'parents': 2,
                'heap type': 'Biparental Binary Heap',
                'insert function': p3.perform_biparental_ascending_swap_and_get_time,
                'insert bound': 'square root',
                'search function': p3.biparental_search_for_key_and_get_index_and_time,
                'search bound': 'square root',
                'max function': p3.get_biparental_max_and_time,
                'max bound': 'square root',
                'extract function': p3.perform_biparental_descending_swap_and_get_time,
                'extract bound': 'square root'
            }
        ]

        heap_size = 1000
        for config in parent_configuration:
            insert_data = [None, [], []]
            search_data = [None, [], []]
            max_data = [None, [], []]
            extract_data = [None, [], []]
            # insert & search
            heap = []
            insert_data[0] = config['insert bound']
            search_data[0] = config['search bound']
            for n in range(heap_size):
                key = random.randrange(1, heap_size, 1)
                heap.append(key)
                time = config['insert function'](heap, len(heap) - 1, 1)
                insert_data[1].append(time)
                insert_time_bound = get_time_bound(heap, config['insert bound'])
                self.assertLessEqual(time, insert_time_bound)
                insert_data[2].append(insert_time_bound)
                search_time_bound = get_time_bound(heap, config['search bound'])
                if config['search bound'] == 'square root':
                    # M,S: "... the cost for a search is at most 2 * sqrt(2 * n) + O(1) comparisons"
                    search_time_bound *= 2
                _, time = config['search function'](heap, key)
                self.assertLessEqual(time, search_time_bound)
                search_data[1].append(time)
                search_data[2].append(search_time_bound)
            # max
            max_data[0] = config['max bound']
            for n in range(heap_size):
                max_time_bound = get_time_bound(heap[:n], config['max bound'])
                _, time = config['max function'](heap[:n])
                self.assertLessEqual(time, max_time_bound)
                max_data[1].append(time)
                max_data[2].append(max_time_bound)
            # extract
            extract_data[0] = config['extract bound']
            for n in range(heap_size):
                extract_time_bound = get_time_bound(heap, config['extract bound'])
                if len(heap) > 1:
                    heap[0], heap[-1] = heap[-1], heap[0]
                    heap.pop(-1)
                time = config['extract function'](heap, 0, 1)
                self.assertLessEqual(time, extract_time_bound)
                extract_data[1].append(time)
                extract_data[2].append(extract_time_bound)
            plot_data(config['heap type'], heap_size, insert_data, search_data, max_data, extract_data)


def plot_data(heap_type, heap_size, insert_data, search_data, max_data, extract_data):
    fig, axs = plt.subplots(2, 2)
    fig.tight_layout(h_pad=2)
    heap_size_range = range(heap_size)
    axs[0, 0].scatter(heap_size_range, insert_data[1], marker='.')
    axs[0, 0].scatter(heap_size_range, insert_data[2], marker='.')
    axs[0, 0].set_title('insert (' + insert_data[0] + ')')
    axs[0, 1].scatter(heap_size_range, search_data[1], marker='.')
    axs[0, 1].scatter(heap_size_range, search_data[2], marker='.')
    axs[0, 1].set_title('search (' + search_data[0] + ')')
    axs[1, 0].scatter(heap_size_range, extract_data[1], marker='.')
    axs[1, 0].scatter(heap_size_range, extract_data[2], marker='.')
    axs[1, 0].set_title('extract (' + extract_data[0] + ')')
    axs[1, 1].scatter(heap_size_range, max_data[1], marker='.')
    axs[1, 1].scatter(heap_size_range, max_data[2], marker='.')
    axs[1, 1].set_title('max (' + max_data[0] + ')')
    fig.text(0.5, 0.01, 'Heap Size (number of keys)', ha='center')
    fig.text(0.01, 0.5, 'Time Taken (number of recursions or iterations)', va='center', rotation='vertical')
    fig.suptitle(heap_type)
    plt.subplots_adjust(top=0.9)
    plt.savefig(heap_type.replace(' ', '_') + '.png')
    plt.show()
