from otree.api import Currency as c, currency_range
from . import *
from otree.api import Bot



class PlayerBot(Bot):

    def play_round(self):
        yield Introduction

        yield Introduction, dict(accept_info = True)
        yield Checks, dict(
            serious=1,
            lose_interest=1,
            cared=1,
            randomization=1,
            self_select_manager=1,
            concerned_about_honest=1,
            attractive_dishonesty=1
        )
