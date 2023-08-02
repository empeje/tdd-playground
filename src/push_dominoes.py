class PushDominoes(object):
    def push_dominoes(self, dominoes):
        forces = [0] * len(dominoes)
        max_force = len(dominoes)

        force = 0
        for i, d in enumerate(dominoes):
            if d == 'R':
                force = max_force
            if d == 'L':
                force = 0
            else:
                force = max(0, force-1)
            forces[i] = force

        for i in range(len(dominoes) - 1, -1, -1):
            d = dominoes[i]
            if d == 'L':
                force = max_force
            if d == 'R':
                force = 0
            else:
                force = max(0, force-1)
            forces[i] -= force

        result = ''
        for f in forces:
            if f == 0:
                result += "."
            elif f > 0:
                result += "R"
            elif f < 0:
                result += "L"

        return result

