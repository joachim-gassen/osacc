import time

from otree.api import *


doc = """
Instructions
"""


class C(BaseConstants):
    NAME_IN_URL = 'instructions'
    PLAYERS_PER_GROUP = None
    NUM_ROUNDS = 1


class Subsession(BaseSubsession):
    pass

class Group(BaseGroup):
    pass


class Player(BasePlayer):
    # drop_out = models.BooleanField(initial=False)

    # Control questions for instructions (removed blank=False)
    confirm_one = models.IntegerField(initial=0, min=0, max=1, blank=True)

# FUNCTIONS
# def creating_session(subsession: Subsession):
#     for p in subsession.get_players():
#         p.participant.dofus = False

# PAGES
# class Anonymous(Page):
#     @staticmethod
#     def is_displayed(player: Player):
#         import random
#
#         global TAKEN_LABELS  # stored in RAM outside of oTree
#
#         try:
#             if player.participant.label is None:
#                 # runs if one is not using a room
#                 return False
#             if player.participant.label in TAKEN_LABELS:
#                 # this indicates that player is a duplicate
#                 player.participant.label = None  # optional: gives even duplicates some privacy
#                 player.participant.dofus = True
#                 return False
#         except NameError:
#             # runs if this is the first subject
#             TAKEN_LABELS = list()
#
#         TAKEN_LABELS.append(player.participant.label)  # mark subject as having participated
#         random.shuffle(TAKEN_LABELS)  # improves anonymity even if this variable were read
#
#         player.participant.label = None  # overwrite label
#
#         return False
#
# class Cleaning(Page):
#     @staticmethod
#     def is_displayed(player: Player):
#         return (
#                 player.participant.dofus == True
#         )
#
#     @staticmethod
#     def app_after_this_page(player: Player, upcoming_apps):
#             return upcoming_apps[-1]

class Welcome(Page):
    form_model = 'player'
    form_fields = ['confirm_one']

class Instruct_one(Page):
    # form_model = 'player'
    # form_fields = ['confirm_one']
    pass

page_sequence = [
    # Anonymous,
    # Cleaning,
    Welcome,
    Instruct_one,
]
