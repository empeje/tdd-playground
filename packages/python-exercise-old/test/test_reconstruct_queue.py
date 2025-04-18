from src.reconstruct_queue import ReconstructQueue


def test_reconstruct_queue():
    input = [[7, 0], [4, 4], [7, 1], [5, 0], [6, 1], [5, 2]]
    assert ReconstructQueue().recon(input) == [[5, 0], [7, 0], [5, 2], [6, 1], [4, 4], [7, 1]]
