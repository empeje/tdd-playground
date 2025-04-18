class ReconstructQueue:
    def recon(self, input):
        input.sort(key = lambda x:
            (-x[0], x[1])
        )
        res = []
        for person in input:
            res.insert(person[1], person)
        return res
