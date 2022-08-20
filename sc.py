# Kyle Maclean
# Seam Carving with Dynamic Programming
# November 2021

# Usage: python sc.py path/input.png path/output.png n_seams

from sys import argv
import cv2
import numpy as np


def get_sobel_gradient(grey):
    # https://docs.opencv.org/4.x/d2/d2c/tutorial_sobel_derivatives.html
    x_gradient = cv2.Sobel(grey, cv2.CV_16S, 1, 0, ksize=3, scale=1, delta=0, borderType=cv2.BORDER_REPLICATE)
    y_gradient = cv2.Sobel(grey, cv2.CV_16S, 0, 1, ksize=3, scale=1, delta=0, borderType=cv2.BORDER_REPLICATE)
    return np.absolute(x_gradient) + np.absolute(y_gradient)


def get_absolute_seam(e):
    # https://dl.acm.org/doi/10.1145/1276377.1276390
    rows, cols = e.shape
    M_indices = np.empty((rows, cols), dtype=np.int32)
    M = np.copy(e)
    middle_indices_offset = np.arange(cols - 2)
    for row in range(rows):
        # M(i, 0) = e(i, 0) + min(M(i−1, 0), M(i−1, 1))
        left_min_index = np.argmin(M[row][:2], axis=0)
        # M(i, width) = e(i, width) + min(M(i−1, width−1), M(i−1, width))
        right_min_index = np.argmin(M[row][-2:], axis=0) + (cols - 2)
        # M(i, j) = e(i, j) + min(M(i−1, j−1), M(i−1, j), M(i−1, j+1))
        middle_min_indices = np.argmin([M[row][:-2], M[row][1:-1], M[row][2:]], axis=0) + middle_indices_offset
        M_indices[row] = np.concatenate(([left_min_index], middle_min_indices, [right_min_index]))
        if row == rows - 1:
            break
        M[row + 1] = e[row + 1] + M[row][M_indices[row]]
    min_cols = [-1] * rows
    min_cols[-1] = int(np.argmin(M[-1]))
    for row in reversed(range(1, rows)):
        min_cols[row - 1] = M_indices[row - 1, min_cols[row]]
    return [(min_cols[row] + row * cols) for row in range(rows)]


def get_carved_image(bgr, n_seams):
    h, w, c = bgr.shape
    grey = cv2.cvtColor(bgr, cv2.COLOR_BGR2GRAY)
    b = bgr[:, :, 0]
    g = bgr[:, :, 1]
    r = bgr[:, :, 2]
    energy = get_sobel_gradient(grey)
    for seam_i in range(n_seams):
        absolute_seam = get_absolute_seam(energy)
        grey = np.delete(grey, absolute_seam).reshape((h, w - seam_i - 1))
        b = np.delete(b, absolute_seam).reshape((h, w - seam_i - 1))
        g = np.delete(g, absolute_seam).reshape((h, w - seam_i - 1))
        r = np.delete(r, absolute_seam).reshape((h, w - seam_i - 1))
        relative_seam = [col % (w - seam_i) for col in absolute_seam]
        beyond_left_seam_span = max(0, min(relative_seam) - 2)
        beyond_right_seam_span = min(w - seam_i, max(relative_seam) + 2)
        seam_width_e = get_sobel_gradient(grey[:, beyond_left_seam_span:beyond_right_seam_span])
        if beyond_left_seam_span != 0:
            seam_width_e = seam_width_e[:, 1:]
            beyond_left_seam_span += 1
        if beyond_right_seam_span != w - seam_i:
            seam_width_e = seam_width_e[:, :-1]
            beyond_right_seam_span -= 1
        energy = np.hstack((energy[:, :beyond_left_seam_span], seam_width_e, energy[:, beyond_right_seam_span + 1:]))
    return np.stack([b, g, r], axis=2)


if __name__ == '__main__':
    cv2.imwrite(argv[2], get_carved_image(cv2.imread(argv[1]), int(argv[3])).astype(np.uint8))
