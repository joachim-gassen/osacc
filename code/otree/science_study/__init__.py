from otree.api import *
import itertools

doc = """
Your app description
"""


class C(BaseConstants):
    NAME_IN_URL = 'science_study'
    PLAYERS_PER_GROUP = None
    NUM_ROUNDS = 1


class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass


class Player(BasePlayer):
    # control_treatment = models.IntegerField(initial=None, min=0, max=1)
    # private_treatment = models.IntegerField(initial=None, min=0, max=1)
    # journal_treatment = models.IntegerField(initial=None, min=0, max=1)
    share = models.IntegerField(initial=None, blank=False, max=100, min=0)
    check_share = models.IntegerField(initial=None, blank=True)

# FUNCTIONS
def creating_session(subsession: Subsession):
    randomdraw = itertools.cycle([1, 2, 3, 4, 5, 6])
    randomdrawp = itertools.cycle([1, 2, 3, 4])
    c_random = itertools.cycle([0, 1])
    p_random = itertools.cycle([0, 1])
    j_random = itertools.cycle([0, 1])

    #assign treatment based on config OR balanced randomization
    for p in subsession.get_players():
        if subsession.session.config['full_random'] == 0:
            # Assign control
            if subsession.session.config['control'] == 1:
                p.participant.control_treatment = 1
            elif subsession.session.config['control'] == 0:
                p.participant.control_treatment = 0
            elif subsession.session.config['control'] is None:
                p.participant.control_treatment = next(c_random)

            # Assign private
            if subsession.session.config['private'] == 1:
                p.participant.private_treatment = 1
            elif subsession.session.config['private'] == 0:
                p.participant.private_treatment = 0
            elif subsession.session.config['private'] is None:
                p.participant.private_treatment = next(p_random)

            # Assign journal
            if subsession.session.config['journal'] == 1:
                p.participant.journal_treatment = 1
            elif subsession.session.config['journal'] == 0:
                p.participant.journal_treatment = 0
            elif subsession.session.config['journal'] is None:
                p.participant.journal_treatment = next(j_random)
        elif subsession.session.config['full_random'] == 1:
            treat = next(randomdraw)
            if treat == 1:
                p.participant.control_treatment = 1
                p.participant.private_treatment = 1
                p.participant.journal_treatment = 0
            elif treat == 2:
                p.participant.control_treatment = 1
                p.participant.private_treatment = 0
                p.participant.journal_treatment = 0
            elif treat == 3:
                p.participant.control_treatment = 0
                p.participant.private_treatment = 1
                p.participant.journal_treatment = 0
            elif treat == 4:
                p.participant.control_treatment = 0
                p.participant.private_treatment = 0
                p.participant.journal_treatment = 0
            elif treat == 5:
                p.participant.control_treatment = 1
                p.participant.private_treatment = 0
                p.participant.journal_treatment = 1
            elif treat == 6:
                p.participant.control_treatment = 0
                p.participant.private_treatment = 0
                p.participant.journal_treatment = 1
        elif subsession.session.config['full_random'] == 0.5:
            treat = next(randomdrawp)
            if treat == 1:
                p.participant.control_treatment = 1
                p.participant.private_treatment = 1
                p.participant.journal_treatment = 0
            elif treat == 2:
                p.participant.control_treatment = 1
                p.participant.private_treatment = 0
                p.participant.journal_treatment = 0
            elif treat == 3:
                p.participant.control_treatment = 0
                p.participant.private_treatment = 1
                p.participant.journal_treatment = 0
            elif treat == 4:
                p.participant.control_treatment = 0
                p.participant.private_treatment = 0
                p.participant.journal_treatment = 0

# PAGES
class Decision(Page):
    form_model = 'player'
    form_fields = ['share', 'check_share']

    @staticmethod
    def error_message(player: Player, value):
        # if self.group.r == None:
        if value["check_share"] == None:
            return 'Please drag the slider to make a decision.'

    def vars_for_template(player: Player):
        return dict(
            control_treatment=player.participant.control_treatment,
            private_treatment=player.participant.private_treatment,
            journal_treatment=player.participant.journal_treatment
        )

    def before_next_page(player: Player, timeout_happened):
        player.participant.share = player.share

page_sequence = [Decision]
