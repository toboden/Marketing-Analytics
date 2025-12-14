import librosa
import pandas as pd
import numpy as np
import openpyxl
import os
import unicodedata

audio_dir = '/Users/philippgrunenberg/Downloads/download'
audio_features_dir = 'data/audio_features.xlsx'
ranked_songs_dir = 'data/Songs mit Ranking_100 Songs.xlsx'


def clean_title(title):
    # Finde das erste Vorkommen von '(' oder '?'
    for char in ['(', '?', '/']:
        if char in title:
            title = title.split(char)[0]  # Nur der Teil vor dem Sonderzeichen
            break
    title = unicodedata.normalize('NFKD', title).encode('ASCII', 'ignore').decode('utf-8')
    return title.strip().lower()

def rename_files(file_list, track_table):
    renamed = 0
    for title, (artist, rating) in track_table.items():
 
        clean_track = clean_title(title)
        clean_artist = clean_title(artist)
        rating_str = str(rating) if pd.notna(rating) else 'x'
        
        # Zuerst nach Titel matchen
        match = next((song for song in file_list if clean_track in clean_title(song)), None)
        if not match:
            # Dann nach Künstler matchen
            match = next((song for song in file_list if clean_artist in clean_title(song)), None)
            if match:
                print(f'Use artist to match: {artist}: {title} -> {match}')
            else:
                print(f'Nicht gefunden: {artist}: {title}')
                continue
            
            
            
        new_name = f'{clean_artist}_{clean_track}_{rating_str}.wav'
        new_path = os.path.join(audio_dir, new_name)
        old_path = os.path.join(audio_dir, match)
        
        try:
          os.rename(old_path, new_path)
          renamed = renamed + 1
          file_list.remove(match)
        except:
          print(f'Error: could not rename {match} to {new_name}')
          
            
    print(f'renamed {renamed} files.')
    print(f'did not rename {len(file_list)} files: ')
    for file in file_list:
      print(file)

def extract_features(file_path):
    y, sr = librosa.load(file_path, sr=None)

    # MFCC
    mfcc = librosa.feature.mfcc(y=y, sr=sr, n_mfcc=13)
    # Spektrale Features
    spec_centroid = librosa.feature.spectral_centroid(y=y, sr=sr)
    zcr = librosa.feature.zero_crossing_rate(y)
    rms = librosa.feature.rms(y=y)
    chroma = librosa.feature.chroma_stft(y=y, sr=sr)
    spec_bandwidth = librosa.feature.spectral_bandwidth(y=y, sr=sr)
    rolloff = librosa.feature.spectral_rolloff(y=y, sr=sr)
    flatness = librosa.feature.spectral_flatness(y=y)
    tempo, _ = librosa.beat.beat_track(y=y, sr=sr)

    features = {}

    # MFCCs: Mean & Std
    for i in range(13):
        features[f'mfcc_{i+1}_mean'] = mfcc[i].mean()
        features[f'mfcc_{i+1}_std'] = mfcc[i].std()

    # Weitere Features
    features['spectral_centroid_mean'] = spec_centroid.mean()
    features['spectral_centroid_std'] = spec_centroid.std()
    features['spectral_bandwidth_mean'] = spec_bandwidth.mean()
    features['spectral_bandwidth_std'] = spec_bandwidth.std()
    features['rolloff_mean'] = rolloff.mean()
    features['rolloff_std'] = rolloff.std()
    features['flatness_mean'] = flatness.mean()
    features['flatness_std'] = flatness.std()
    features['zcr_mean'] = zcr.mean()
    features['zcr_std'] = zcr.std()
    features['rms_mean'] = rms.mean()
    features['rms_std'] = rms.std()

    # Chroma: Mean & Std pro Tonklasse
    for i in range(chroma.shape[0]):
        features[f'chroma_{i+1}_mean'] = chroma[i].mean()
        features[f'chroma_{i+1}_std'] = chroma[i].std()

    # Tempo
    features['tempo'] = tempo

    return features


# Excel-Datei einlesen
ranked_songs_xlsx = pd.read_excel(ranked_songs_dir)
# Mapping aus der Excel-Tabelle erstellen
track_table = dict(zip(ranked_songs_xlsx['Track Title'], zip(ranked_songs_xlsx['Artist'], ranked_songs_xlsx['Bewertung'])))
# Dateinamen anpassen
#rename_files(os.listdir(audio_dir), track_table)

#features extrahieren
rows = []
for file in os.listdir(audio_dir):
    if file.endswith('.wav'):
        path = os.path.join(audio_dir, file)
        features = extract_features(path)
        features['song'] = file
        rows.append(features)
#write data to excel
feature_df = pd.DataFrame(rows)
feature_df.set_index('song', inplace=True)
feature_df.to_excel(audio_features_dir)
