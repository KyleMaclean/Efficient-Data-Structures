"""
Kyle Maclean
Priority Queues
December 2021

Let:
n be the length of the list representing the biparental heap;
p stand for parent, c stand for child, l stand for left and r stand for right;
M,S stand for Munro, Suwanda, the authors of: _Implicit Data Structures for Fast Search and Update_ (1980).

operation | uniparental binary heap | biparental binary heap
------------------------------------------------------------
min         O(1)                      O(1)
max         O(n)                      O(sqrt(n))
insert      O(log(n))                 O(sqrt(n))
extract     O(log(n))                 O(sqrt(n))
search      O(n)                      O(sqrt(n))
"""
parent_support = 'Only heaps in which nodes have either one or two parents are supported.'


def get_min(heap, _=None):
    """operation: min (returning), time: O(1)"""
    min_key, _ = get_min_and_time(heap)
    return min_key


def get_max(heap, parents):
    """operation: max (returning), time: O(n) if parents == 1 or O(sqrt(n)) if parents == 2"""
    if parents == 1:
        max_key, _ = get_uniparental_max_and_time(heap)
    elif parents == 2:
        max_key, _ = get_biparental_max_and_time(heap)
    else:
        return ValueError(parent_support)
    return max_key


def perform_insertion(heap, key, parents):
    """operation: insert (in-place), time: O(log(n)) if parents == 1 or O(sqrt(n)) if parents == 2"""
    heap.append(key)
    if parents == 1:
        _ = perform_uniparental_ascending_swap_and_get_time(heap, len(heap) - 1, 1)
    elif parents == 2:
        _ = perform_biparental_ascending_swap_and_get_time(heap, len(heap) - 1, 1)
    else:
        return ValueError(parent_support)


def perform_extraction_and_get_min(heap, parents):
    """operation: extract (in-place & returning), time: O(log(n)) if parents == 1 or O(sqrt(n)) if parents == 2"""
    root = None
    if len(heap) == 1:
        root = heap.pop()
    elif len(heap) > 1:
        # Lecture 8: "choose the last element as the new root ..."
        heap[0], heap[-1] = heap[-1], heap[0]
        root = heap.pop(-1)
        # Lecture 8: "... then move it down if necessary"
        if parents == 1:
            _ = perform_uniparental_descending_swap_and_get_time(heap, 0, 1)
        elif parents == 2:
            _ = perform_biparental_descending_swap_and_get_time(heap, 0, 1)
        else:
            return ValueError(parent_support)
    return root


def search_for_key_and_get_index(heap, key, parents):
    """operation: search (returning), time: O(n) if parents == 1 or O(sqrt(n)) if parents == 2"""
    if parents == 1:
        index, _ = uniparental_search_for_key_and_get_index_and_time(heap, key)
    elif parents == 2:
        index, _ = biparental_search_for_key_and_get_index_and_time(heap, key)
    else:
        return ValueError(parent_support)
    return index


def is_valid(heap, parents):
    if not heap:
        return True
    elif parents == 1:
        return uniparental_recursive_validity(heap, 0)
    elif parents == 2:
        return biparental_recursive_validity(heap, 0)
    else:
        return ValueError(parent_support)


def get_min_and_time(heap):
    root = None
    if len(heap) > 0:
        root = heap[0]
    return root, 1


def get_uniparental_max_and_time(heap):
    time = 1
    max_key = None
    if len(heap) > 0:
        max_key = heap[0]
    for key in heap[1:]:
        time += 1
        if key > max_key:
            max_key = key
    return max_key, time


def get_biparental_max_and_time(beap):
    time = 0
    max_key = None
    # M, S: "The maximum is in one of the last sqrt(2n) locations."
    search_size = int((2 * len(beap)) ** (1 / 2))
    if search_size > 0:
        search_space = beap[-search_size:]
        max_key = search_space[0]
        for key in search_space[1:]:
            time += 1
            if key > max_key:
                max_key = key
    return max_key, time


def perform_uniparental_ascending_swap_and_get_time(heap, index, time):
    if index != 0:
        time += 1
        parent_index = (index - 1) // 2
        if heap[parent_index] > heap[index]:
            heap[parent_index], heap[index] = heap[index], heap[parent_index]
        return perform_uniparental_ascending_swap_and_get_time(heap, parent_index, time)
    return time


def perform_biparental_ascending_swap_and_get_time(beap, c_index, time):
    l_p_index, r_p_index = get_biparental_parents(c_index)
    if l_p_index is not None or r_p_index is not None:
        time += 1
        # M,S: "... smaller than either of its parents, it is interchanged with the larger parent"
        if l_p_index is not None:
            if r_p_index is not None:
                max_p_index = r_p_index if beap[l_p_index] < beap[r_p_index] else l_p_index
            else:
                max_p_index = l_p_index
        else:
            max_p_index = r_p_index
        if beap[c_index] < beap[max_p_index]:
            beap[c_index], beap[max_p_index] = beap[max_p_index], beap[c_index]
            return perform_biparental_ascending_swap_and_get_time(beap, max_p_index, time)
    return time


def perform_uniparental_descending_swap_and_get_time(heap, index, time):
    l_child_index = 2 * index + 1
    if index < len(heap) - 1 and l_child_index < len(heap):
        time += 1
        r_child_index = 2 * index + 2
        min_child_index = l_child_index \
            if r_child_index >= len(heap) or heap[r_child_index] > heap[l_child_index] \
            else r_child_index
        if heap[index] > heap[min_child_index]:
            heap[index], heap[min_child_index] = heap[min_child_index], heap[index]
            return perform_uniparental_descending_swap_and_get_time(heap, min_child_index, time)
    return time


def perform_biparental_descending_swap_and_get_time(beap, p_index, time):
    l_c_index, r_c_index = get_biparental_children(p_index, len(beap) - 1)
    if l_c_index is not None or r_c_index is not None:
        time += 1
        # if larger than either child, then interchange with smaller child
        if l_c_index is not None:
            if r_c_index is not None:
                min_c_index = l_c_index if beap[l_c_index] < beap[r_c_index] else r_c_index
            else:
                min_c_index = l_c_index
        else:
            min_c_index = r_c_index
        if min_c_index is not None and beap[p_index] > beap[min_c_index]:
            beap[p_index], beap[min_c_index] = beap[min_c_index], beap[p_index]
            return perform_biparental_descending_swap_and_get_time(beap, min_c_index, time)
    return time


def uniparental_search_for_key_and_get_index_and_time(heap, key):
    time = 1
    for index in range(len(heap)):
        time += 1
        if heap[index] == key:
            return index, time
    return None, time


def biparental_search_for_key_and_get_index_and_time(beap, key):
    time = 1
    # M,S: "... view the structure as an “upper-left” triangular matrix"
    if len(beap) == 0:
        return None, time
    ultimate_level = get_biparental_level(len(beap) - 1)
    penultimate_level = ultimate_level - 1
    _, r_ultimate_edge_index = get_biparental_block_indices(ultimate_level)
    _, r_penultimate_edge_index = get_biparental_block_indices(penultimate_level)
    # M,S: "... start searching for an element ... at the top right corner of the matrix"
    top_right_corner = r_ultimate_edge_index if r_ultimate_edge_index == len(beap) - 1 else r_penultimate_edge_index
    return biparental_recursive_search(beap, key, top_right_corner, time)


def biparental_recursive_search(beap, key, index, time):
    if key == beap[index]:
        return index, time
    mid_level = get_biparental_level(index)
    if mid_level >= 0:
        time += 1
        l_mid_edge_index, _ = get_biparental_block_indices(mid_level)
        column = index - l_mid_edge_index
        top_level = mid_level - 1
        if key < beap[index] and index != l_mid_edge_index and top_level >= 0:
            # M,S: "... move left one position along the row"
            l_top_edge_index, _ = get_biparental_block_indices(top_level)
            new_index = l_top_edge_index + column - 1
            return biparental_recursive_search(beap, key, new_index, time)
        elif key > beap[index]:
            bot_level = mid_level + 1
            new_index = get_biparental_block_indices(bot_level)[0] + column
            if new_index < len(beap):
                # M,S: "... move down one position along the column"
                return biparental_recursive_search(beap, key, new_index, time)
            elif index - 1 >= l_mid_edge_index:
                # M,S: "... move left and down one position each"
                return biparental_recursive_search(beap, key, index - 1, time)
    return None, time


def uniparental_recursive_validity(heap, index):
    if index < len(heap):
        key = heap[index]
        l_child_index = 2 * index + 1
        r_child_index = 2 * index + 2
        l_child_property = True if l_child_index >= len(heap) or heap[l_child_index] >= key else False
        r_child_property = True if r_child_index >= len(heap) or heap[r_child_index] >= key else False
        return (l_child_property and r_child_property
                and uniparental_recursive_validity(heap, l_child_index)
                and uniparental_recursive_validity(heap, r_child_index))
    return True


def biparental_recursive_validity(beap, p_index):
    # checks whether the children of the given parent (if they exist) are at least the size of the parent, recursively
    l_c_index, r_c_index = get_biparental_children(p_index, len(beap) - 1)
    if l_c_index is not None or r_c_index is not None:
        if not (False if l_c_index is not None and beap[l_c_index] < beap[p_index] else True) or not (
                False if r_c_index is not None and beap[r_c_index] < beap[p_index] else True):
            return False
        if l_c_index is not None and r_c_index is not None:
            return biparental_recursive_validity(beap, l_c_index) and biparental_recursive_validity(beap, r_c_index)
        if l_c_index is not None:
            return biparental_recursive_validity(beap, l_c_index)
        if r_c_index is not None:
            return biparental_recursive_validity(beap, r_c_index)
    return True


def get_biparental_block_indices(level):
    # calculate which indices in the triangular beap's one-dimensional representation form the edges of the given level
    l_edge_index = (level ** 2 + level) // 2
    r_edge_index = (level ** 2 + 3 * level) // 2
    return l_edge_index, r_edge_index


def get_biparental_level(index):
    # calculate the level of the triangular beap where the specified index can be found
    if index < 0:
        raise ValueError('Cannot get the level of a negative index.')
    if index == 0:
        return 0
    level = (2 * index) ** (1 / 2)
    level = int(level)
    l_edge_index, r_edge_index = get_biparental_block_indices(level)
    # dealing with off-by-one errors
    if index in range(l_edge_index, r_edge_index):
        return level
    else:
        return level - 1


def get_biparental_children(p_index, max_index):
    # retrieves two, one or no indices of the given parent's children according to whether they exist
    p_level = get_biparental_level(p_index)
    c_level = p_level + 1
    ultimate_level = get_biparental_level(max_index)
    if c_level <= ultimate_level:
        p_l_edge_index, _ = get_biparental_block_indices(p_level)
        c_l_edge_index, _ = get_biparental_block_indices(c_level)
        p_column = p_index - p_l_edge_index
        l_c_index = c_l_edge_index + p_column
        r_c_index = c_l_edge_index + p_column + 1
        if l_c_index > max_index:
            l_c_index = None
        if r_c_index > max_index:
            r_c_index = None
        return l_c_index, r_c_index
    return None, None


def get_biparental_parents(c_index):
    # retrieves two, one or no indices of the given child's parents according to whether they exist
    c_level = get_biparental_level(c_index)
    p_level = c_level - 1
    if p_level >= 0:
        c_l_edge_index, c_r_edge_index = get_biparental_block_indices(c_level)
        p_l_edge_index, _ = get_biparental_block_indices(p_level)
        c_column = c_index - c_l_edge_index
        l_p_index = p_l_edge_index + c_column - 1 if c_index > c_l_edge_index else None
        r_p_index = p_l_edge_index + c_column if c_index < c_r_edge_index else None
        return l_p_index, r_p_index
    return None, None
