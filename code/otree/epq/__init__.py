import itertools
import json
import random

from otree.api import *


author = 'Victor van Pelt'
doc = """
Ex-post Questionnaire
"""


class C(BaseConstants):
    NAME_IN_URL = 'epq'
    PLAYERS_PER_GROUP = None
    NUM_ROUNDS = 1
    STANDARDCHOICESFIVE = [
        [1, 'Disagree strongly'],
        [2, 'Disagree'],
        [3, 'Neither agree nor disagree'],
        [4, 'Agree'],
        [5, 'Agree strongly'],
    ]


class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass


class Player(BasePlayer):
    # EPQ 1 - Randomization and reflection
    reflect = models.TextField(
        label="", blank=False
    )
    researcher_feedback = models.TextField(
        label="TESTING ONLY: Do you have any feedback for us about the study?", blank=True
    )

    # EPQ 2 - Demographics
    continent = models.IntegerField(
        label="On which continent do you live?",
        blank=False,
        choices=[
            [0, 'Asia (including the Middle East)'],
            [1, 'Australia and Oceania'],
            [2, 'Africa'],
            [3, 'Central and South America (including the Caribbean)'],
            [4, 'Europe'],
            [5, 'North America'],
        ],
    )
    gender = models.IntegerField(
        label="Please select your gender.",
        blank=False,
        choices=[
            [1, 'Male'],
            [2, 'Female'],
            [3, 'Other'],
        ],
    )
    age = models.IntegerField(
        label="What is your age?",
        choices=[
            [1, 'Under 25 years'],
            [2, '25 to 35 years'],
            [3, '35 to 45 years'],
            [4, '45 to 55 years'],
            [5, '55 to 65 years'],
            [6, '65 years or over'],
        ],
        blank=False)
    risk = models.IntegerField(
        label="Please rate your willingness to take risks in general on a scale from 1 (not at all willing) to 10 (very willing).",
        choices=[
            [1, '1 (not at all willing)'],
            [2, '2'],
            [3, '3'],
            [4, '4'],
            [5, '5'],
            [6, '6'],
            [7, '7'],
            [8, '8'],
            [9, '9'],
            [10, '10 (very willing)'],
        ],
        blank=False,
    )
    trustworthy = models.IntegerField(
        label="If someone does me a favor, I am ready to return it. Please rate how strongly this applies to you on a scale from 1 (does not apply to me at all) to 10 (applies to me completely).",
        choices=[
            [1, '1 (does not apply to me at all)'],
            [2, '2'],
            [3, '3'],
            [4, '4'],
            [5, '5'],
            [6, '6'],
            [7, '7'],
            [8, '8'],
            [9, '9'],
            [10, '10 (applies to me completely)'],
        ],
        blank=False,
    )
    trusting = models.IntegerField(
        label="Generally speaking, would you say that you can’t be too careful in dealing with people or that most people can be trusted? Please rate this on a scale from 1 (you can’t be too careful in dealing with people) to 10 (most people can be trusted).",
        blank=False,
        choices=[
            [1, "1 (one can't be too careful)"],
            [2, '2'],
            [3, '3'],
            [4, '4'],
            [5, '5'],
            [6, '6'],
            [7, '7'],
            [8, '8'],
            [9, '9'],
            [10, '10 (most people can be trusted)'],
        ],
    )
    accounting_research = models.IntegerField(
        label="In your current role, do you carry out academic accounting research?",
        blank=False,
        choices=[
            [0, 'No'],
            [1, 'Yes']
        ],
    )
    job_title = models.IntegerField(
        label="Which of the following job categories best applies to you?",
        blank=False,
        choices=[
            [1, 'PhD student'],
            [2, 'Untenured faculty'],
            [3, 'Tenured faculty'],
            [4, 'Other (e.g., practitioner)']
        ],
    )
    research_area = models.IntegerField(
        label="Which of the following best describes your main subfield within accounting?",
        blank=False,
        choices=[
            [1, 'Auditing'],
            [2, 'Financial accounting and reporting'],
            [3, 'Managerial accounting'],
            [4, 'Taxation'],
            [5, 'Accounting information systems'],
            [6, 'Other']
        ],
    )
    research_method = models.IntegerField(
        label="Which of the following best describes your main methodological area?",
        blank=False,
        choices=[
            [1, 'Analytical'],
            [2, 'Archival/Field'],
            [3, 'Experimental'],
            [4, 'Qualitative'],
            [5, 'Survey'],
            [6, 'Other']
        ],
    )

    # Open Science Questions
    res_meth_replicates = models.IntegerField(
        label="How confident are you that the influential research findings in the area of accounting overall are reproducible?",
        blank=False,
        choices=[
            [1, 'Not at all confident'],
            [2, 'Slightly confident'],
            [3, 'Somewhat confident'],
            [4, 'Moderately confident'],
            [5, 'Extremely confident']
        ],
    )

    acc_replicates = models.IntegerField(
        label="How confident are you that the influential research findings in the area of accounting overall are reproducible?",
        blank=False,
        choices=[
            [1, 'Not at all confident'],
            [2, 'Slightly confident'],
            [3, 'Somewhat confident'],
            [4, 'Moderately confident'],
            [5, 'Extremely confident']
        ],
    )
    acc_sub_replicates = models.IntegerField(
        label="How confident are you that the influential research findings in your main subfield within accounting are reproducible?",
        blank=False,
        choices=[
            [1, 'Not at all confident'],
            [2, 'Slightly confident'],
            [3, 'Somewhat confident'],
            [4, 'Moderately confident'],
            [5, 'Extremely confident']
        ],
    )
    progress = models.IntegerField(
        label="To what extent do you believe that publicly sharing research materials (i.e., data, code, and research procedures) online is important for the advancement of accounting research?",
        blank=False,
        choices=[
            [1, 'Not at all important'],
            [2, 'Moderately not important'],
            [3, 'Neither unimportant nor important'],
            [4, 'Moderately important'],
            [5, 'Very important']
        ],
    )
    est_other_post_online = models.IntegerField(
        label="In your estimation, what percentage of accounting researchers publicly share research materials (i.e., data, code, and research procedures) online?",
        min=0,
        step=1,
        max=100,
        initial=None,
    )
    check_est_other_post_online = models.IntegerField(blank=True, initial=0)
    est_you_post_online = models.IntegerField(
        label="How often do you publicly share research materials (i.e., data, code, and research procedures)?",
        blank=False,
        choices=[
            [1, 'Never'],
            [2, 'Rarely'],
            [3, 'Sometimes'],
            [4, 'Often'],
            [5, 'Always']
        ],
    )
    ## HAS A TRICKY OTHER PART!!
    reason_one = models.IntegerField(initial=0, min=0, max=1, blank=True)
    reason_two = models.IntegerField(initial=0, min=0, max=1, blank=True)
    reason_three = models.IntegerField(initial=0, min=0, max=1, blank=True)
    reason_four = models.IntegerField(initial=0, min=0, max=1, blank=True)
    reason_five = models.IntegerField(initial=0, min=0, max=1, blank=True)
    reason_six = models.IntegerField(initial=0, min=0, max=1, blank=True)
    reason_seven = models.IntegerField(initial=0, min=0, max=1, blank=True)
    reason_eight = models.IntegerField(initial=0, min=0, max=1, blank=True)
    reason_nine = models.IntegerField(initial=0, min=0, max=1, blank=True)
    reason_eleven = models.IntegerField(initial=0, min=0, max=1, blank=True)
    reason_twelve = models.IntegerField(initial=0, min=0, max=1, blank=True)
    reason_ten = models.IntegerField(initial=0, min=0, max=1, blank=True)
    reason_ten_text = models.StringField(blank=True)

    likelihood_future_post = models.IntegerField(
        label="Do you plan to publicly share research materials (i.e., data, code, and research procedures) in the future?",
        blank=False,
        choices=[
            [1, "No"],
            [2, 'Yes']
        ]
    )
    heard_materials = models.IntegerField(
        label="How familiar are you with the practice of publicly sharing research materials (i.e., data, code, and research procedures) for a completed study?",
        blank=False,
        choices=[
            [1, 'Not at all familiar'],
            [2, 'Slightly familiar'],
            [3, 'Somewhat familiar'],
            [4, 'Moderately familiar'],
            [5, 'Extremely familiar']
        ],
    )

    encourage_others = models.IntegerField(
        label="Do you encourage others to publicly share research materials (i.e., data, code, and research procedures)?",
        blank=False,
        choices=[
            [1, "No, and I don't plan to"],
            [2, 'No, but I plan to in the future'],
            [3, "Yes, I do"]
        ]
    )
    acc_favor_public_post = models.IntegerField(
        label="In your estimation, what percentage of accounting researchers favor sharing research materials (i.e., data, code, and research procedures) publicly?",
        min = 0,
        step = 1,
        max = 100,
        initial = None,
    )
    check_acc_favor_public_post = models.IntegerField(blank=True, initial=0)

    heard_prereg = models.IntegerField(
        label="How familiar are you with the practice of pre-registering hypotheses or analyses in advance of a study?",
        blank=False,
        choices=[
            [1, 'Not at all familiar'],
            [2, 'Slightly familiar'],
            [3, 'Somewhat familiar'],
            [4, 'Moderately familiar'],
            [5, 'Extremely familiar']
        ]
    )
    have_prereg = models.IntegerField(
        label="Have you ever pre-registered hypotheses or analyses in advance of a study?",
        blank=False,
        choices=[
            [1, "No"],
            [2, 'Yes']
        ]
    )
    first_time_prereg = models.IntegerField(
        label="Approximately when was the first time you pre-registered hypotheses or analyses in advance of a study?",
        blank=True,
        initial=None,
        choices=[
            [1996, "1996"],
            [1997, "1997"],
            [1998, "1998"],
            [1999, "1999"],
            [2000, "2000"],
            [2001, "2001"],
            [2002, "2002"],
            [2003, "2003"],
            [2004, "2004"],
            [2005, "2005"],
            [2006, "2006"],
            [2007, "2007"],
            [2008, "2008"],
            [2009, "2009"],
            [2010, "2010"],
            [2011, "2011"],
            [2012, "2012"],
            [2013, "2013"],
            [2014, "2014"],
            [2015, "2015"],
            [2016, "2016"],
            [2017, "2017"],
            [2018, "2018"],
            [2019, "2019"],
            [2020, "2020"],
            [2021, "2021"],
            [2022, "2022"],
        ]
    )
    trust_prereg = models.IntegerField(
        label="How trustworthy do you find the work of scholars who tend to pre-register hypotheses or analyses as compared to the work of those who don't?",
        blank=False,
        choices=[
            [1, 'It is much less trustworthy'],
            [2, 'It is somewhat less trustworthy'],
            [3, 'Equally trustworthy'],
            [4, 'It is somewhat more trustworthy'],
            [5, 'It is much more trustworthy']
        ],
    )
    editors_believe = models.IntegerField(
        label="If you had to guess, approximately what percentage of editors of the top 6 accounting journals believe that research transparency practices increase the value of a manuscript?",
        min=0,
        max=100,
        initial=None
    )
    check_editors_believe = models.IntegerField(blank=True, initial=0)
# FUNCTIONS

# PAGES
class Checks(Page):
    form_model = 'player'
    form_fields = [
        'reflect'
    ]

    def vars_for_template(player: Player):
        return dict(
            share=player.participant.share,
            control_treatment = player.participant.control_treatment,
            private_treatment = player.participant.private_treatment,
            journal_treatment = player.participant.journal_treatment
        )

class Introduction(Page):
    pass

class Science_one(Page):
    form_model = 'player'
    form_fields = [
        'progress',
        'heard_materials',
        'est_other_post_online',
        'est_you_post_online',
        'reason_one',
        'reason_two',
        'reason_three',
        'reason_four',
        'reason_five',
        'reason_six',
        'reason_seven',
        'reason_eight',
        'reason_nine',
        'reason_eleven',
        'reason_twelve',
        'reason_ten',
        'reason_ten_text',
        'likelihood_future_post',
        'encourage_others',
        'acc_favor_public_post',
        'editors_believe',
        'check_est_other_post_online',
        'check_acc_favor_public_post',
        'check_editors_believe'
    ]


class Science_two(Page):
    form_model = 'player'
    form_fields = [
        'heard_prereg',
        'have_prereg',
        'first_time_prereg',
        'trust_prereg',
        'acc_replicates',
        'research_area',
        'acc_sub_replicates',
        'research_method',
        'res_meth_replicates',
    ]

class Demographics(Page):
    form_model = 'player'
    form_fields = [
        'gender',
        'age',
        'continent',
        'accounting_research',
        'job_title',
        'trusting',
        'trustworthy',
        'risk'
    ]

class Feedback(Page):
    form_model = 'player'
    form_fields = ['researcher_feedback']

class Thank(Page):
    pass

page_sequence = [
    Checks,
    Introduction,
    Science_one,
    Science_two,
    # Science_three,
    Demographics,
    # Feedback,
    Thank
]
