{
 "cells": [
  {
   "cell_type": "code",
   "execution_count": 19,
   "id": "3119264a-00f9-4c8f-8b96-5abaabf60c36",
   "metadata": {},
   "outputs": [],
   "source": [
    "import numpy as np"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 20,
   "id": "2ca90d30-fc1c-46fb-b75f-4a99156a71e5",
   "metadata": {},
   "outputs": [],
   "source": [
    "# Min stragei er at jeg plasserer 2 bataljoner i første omgang, 32 på de neste tre rundene, bare 1 bataljoner på de siste også tilsutt \n",
    "#tilfeldig plassering av bataljonene. \n",
    "\n",
    "def player_strategy(n_battalions,n_fields):\n",
    "    battalions=np.zeros(n_fields,dtype=int)\n",
    "    \n",
    "    battalions[0:1]=2\n",
    "    battalions[1:4]=32\n",
    "    battalions[4:]=1\n",
    "\n",
    "    battalions=battalions[np.random.rand(n_fields).argsort()]\n",
    "    assert sum(battalions)==n_battalions\n",
    "    \n",
    "    return battalions"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 21,
   "id": "6040cfd0-2616-402a-a745-b82636f88e70",
   "metadata": {},
   "outputs": [],
   "source": [
    "def computer_strategy(n_battalions,n_fields):\n",
    "    battalions=np.zeros(n_fields,dtype=int)\n",
    "    battalions[0:1]=8\n",
    "    battalions[1:4]=30\n",
    "    battalions[4:]=1\n",
    "    assert sum(battalions)==n_battalions\n",
    "    return battalions"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 22,
   "id": "885b7af0-de0c-4ad0-9b53-fbf8bfdf0de8",
   "metadata": {},
   "outputs": [],
   "source": [
    "def call_battle(n_battalions,n_fields, player_strategy, computer_strategy):\n",
    "    c_battlions=computer_strategy(n_battalions,n_fields)\n",
    "    p_battlions=player_strategy(n_battalions,n_fields)\n",
    "\n",
    "    diff=p_battlions-c_battlions\n",
    "    points=sum(diff>0)-sum(diff<0)\n",
    " \n",
    "    return int(points>0)-int(points<0)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 23,
   "id": "72075b73-ccf8-4e5f-b2f6-ff8b2b096446",
   "metadata": {},
   "outputs": [],
   "source": [
    "def test_strategies(n_fields,n_battalions,player_strategy, computer_strategy):\n",
    "    n_tests=100000\n",
    "    r=0\n",
    "    record=[]\n",
    "    for i in range(n_tests):\n",
    "        p=call_battle(n_battalions,n_fields,\n",
    "            player_strategy, computer_strategy)\n",
    "        record.append(p)\n",
    "        r+=p\n",
    "    return r/n_tests"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 24,
   "id": "bd123741-4e37-4f3c-a433-acab4ed11126",
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "0.80136"
      ]
     },
     "execution_count": 24,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "test_strategies(6,100,player_strategy, computer_strategy)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "id": "1131def2-9ff9-41df-b528-db39e4d499b9",
   "metadata": {},
   "outputs": [],
   "source": []
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Python 3 (ipykernel)",
   "language": "python",
   "name": "python3"
  },
  "language_info": {
   "codemirror_mode": {
    "name": "ipython",
    "version": 3
   },
   "file_extension": ".py",
   "mimetype": "text/x-python",
   "name": "python",
   "nbconvert_exporter": "python",
   "pygments_lexer": "ipython3",
   "version": "3.9.9"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 5
}
